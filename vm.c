#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>

#include "vm.h"
#include "_generated.h"

struct ClassInfo* ClassInfos[256];

word Here, SymVecLen, ClassVecLen;
byte Memory[0x10000];
word SymVec[0x100];
word ClassVec[0x100];
word pc, fp, sp, tmp;

word nilAddr = 0;
word falseAddr = 2;
word trueAddr = 4;
word intAddr;
word clsAddr;

word here;
word gcroot;
word clsroot;

int RAISE(const char* s) {
    fprintf(stderr, "\n*** RAISE: %s\n", s);
    abort();
    return 0;
}

void PrintStr(word str, const char* label) {
        assert(str);
        //Hex20("str", -1, str);
        word guts = WF(str, SLICE_P_guts);
        assert(guts);
        //Hex20("guts", -1, guts);
        word begin = BF(str, SLICE_B_begin);
        word len = BF(str, SLICE_B_len);
        //Hex20("begin/len", len, FlexAddrAt(guts, begin));
        assert(len);
        fprintf(stderr, "    %s: ", label);
        for (word i = 0; i < len; i++) {
          byte ch = BytAt(guts, begin+i);
          bool pr = (' '<=ch && ch <127);
          fprintf(stderr, "%c", pr? ch: '.');
        }
        fprintf(stderr, "\n");
}
void PrintSymNum(byte num, const char* label) {
  word sym = SymVec[num];
  PrintStr(sym, label);
}

void Inspect(word w, const char* msg) {
  word raw_size = 2+2*(word)BF(w, UR_B_gcsize);
  assert(4 <= raw_size);
  assert(raw_size <= 256);
  word cnum = BF(w, UR_B_cls);
  assert(1 <= cnum);
  assert(cnum < 64);

  struct ClassInfo* ci = ClassInfos[cnum];
  if (!ci) {
    fprintf(stderr, "((( %s ))) [ $%04x isa #$%x No ClassInfo ]\n", msg, w, cnum);
    Hex20(msg, cnum, w);
    return;
  }
  word cls = ClassVec[cnum];
  assert(ci->num == cnum);

  word base_size =  BF(cls, CLS_B_bSize);
  word flex_size =  raw_size - base_size;
  fprintf(stderr, "((( %s ))) [ $%04x isa %s(#$%x $%04x)\n", msg, w, ci->name, cnum, cls);
  fprintf(stderr, "  size=%d. base=%d. flex=%d.\n",
      raw_size, base_size, flex_size);
  for (struct FieldInfo *f = ci->fields; f->name; f++) {
    if (f->width == 1) {
      fprintf(stderr, "  { B@%d. %s=$%02x=%d. }\n",
          f->offset, f->name,
          BF(w, f->offset),
          BF(w, f->offset));
      if (!strcmp("B_name", f->name)) {
        word num = BF(w, f->offset);
        assert(num);
        word sym = SymVec[num];
        assert(sym);
        Hex20("sym", num, sym);
        word guts = WF(sym, SLICE_P_guts);
        assert(guts);
        Hex20("guts", -1, guts);
        word begin = BF(sym, SLICE_B_begin);
        word len = BF(sym, SLICE_B_len);
        Hex20("begin/len", len, FlexAddrAt(guts, begin));
        assert(len);
        fprintf(stderr, "    %s: ", f->name);
        for (word i = 0; i < len; i++) {
          byte ch = BytAt(guts, begin+i);
          bool pr = (' '<=ch && ch <127);
          fprintf(stderr, "%c", pr? ch: '.');
        }
        fprintf(stderr, "\n");
      }
      if (!strcmp("B_cls", f->name) ||
          !strcmp("B_owner", f->name)) {
        word num = BF(w, f->offset);
        assert(num);
        struct ClassInfo* xci = ClassInfos[num];
        assert(xci);
      fprintf(stderr, "    %s: %s ($%04x)\n", f->name, xci->name, ClassVec[num]);
      }
    } else if (f->width == 2) {
      fprintf(stderr, "  { W@%d. %s=$%02x=%d. }\n",
          f->offset, f->name,
          WF(w, f->offset),
          WF(w, f->offset));
    } else {
          assert(0);
    }
  }
  if (flex_size) {
  for (word i=0; i<raw_size; i++) {
    if (i == base_size) 
      fprintf(stderr, " : ");  // mark before flex.
    fprintf(stderr, " %02x", BF(w, i));
  }
  fprintf(stderr, " : ");
  for (word i=0; i<raw_size; i++) {
    if (i >= base_size)  {
      byte ch = BF(w, i);
      bool pr = (' '<=ch && ch <127);
      fprintf(stderr, "%c", pr? ch: '.');
    }
  }
  fprintf(stderr, " ;\n");
  }
  fprintf(stderr, "]\n");
}

void Hex20(const char* s, int d, word p) {
	fprintf(stderr, "[%s $%04x=%d.] %04x: ", s, d, d, p);
  if (p != 0xFFFF) {
	for (int i = 0; i < 20; i++) {
		byte b = B(p+i);
		fprintf(stderr, "%02x ", b);
		if ((i&3)==3) fprintf(stderr, " ");
	}
	for (int i = 0; i < 20; i++) {
		byte b = B(p+i);
		fprintf(stderr, "%c", (' '<=b && b<='~' ? b : '.'));
	}
  }
	fprintf(stderr, "\n");
	fflush(stderr);
}

void fprintN(FILE* fd, char* message, word ptr, byte len) {
	char* p = malloc(len + 1);
	for (byte i = 0; i < len; i++) {
		p[i] = B(ptr + i);
	}
	p[len] = '\0';
	fprintf(fd, "%s <%s>\n", message, p);
	free(p);
}
#if 0
void fprintSymNum(FILE* fd, char* message, byte symNum) {
	word sym = SymVec[symNum];
	word guts = W(sym + SLICE_P_guts);
	byte begin = B(sym + SLICE_B_begin);
	byte len = B(sym + SLICE_B_len);

    //word sz = ((B(guts+URCLS_B_gcsize)&127)<<1)+2;
		//Hex20("sz", sz, guts+CLS_FLEXSIZE);
    //word final = B(guts+sz-1);
		//Hex20("final", final, guts+CLS_FLEXSIZE);
    //word name_len = sz - CLS_FLEXSIZE - (final==0);

	//Hex20("(sym)", symNum, sym);
	//Hex20("(guts)", symNum, guts);
	fprintN(fd, message, guts + ARRBYT_FLEXSIZE + begin, len);
}
#endif

;;;;;;;;;;;;;;;;;;;;;;;;;

word BytLen(word p) {
    word pcls = CLASSOF(p);
    word bSize = BF(pcls, CLS_B_bSize);
    word rawLen = 2+2*(word)BF(p, UR_B_gcsize);
    return rawLen-bSize;
}
word PtrLen(word p) {
    return BytLen(p) >> 1;
}
word FlexAddrAt(word p, word i) {
    //Hex20("FlexAddrAt: p", p, p);
    //Inspect(p, "--FlexAddrAt--");
    word pcls = CLASSOF(p);
    //Inspect(pcls, "--cls--");
    //fprintf(stderr, "p: %x i: %x pcls: %x\n", p, i, pcls);
    word bSize = BF(pcls, CLS_B_bSize);
    word rawLen = 2+2*(word)BF(p, UR_B_gcsize);
    word flex = rawLen - bSize;
    //fprintf(stderr, "bSize: %x rawLen: %x flex: %x\n", bSize, rawLen, flex);
    if (i >= flex) RAISE("FlexAddrAt/BadIndex");
    return p + bSize + i;
}
byte BytAt(word p, word i) {
    return B(FlexAddrAt(p, i));
}
word PtrAt(word p, word i) {
    return W(FlexAddrAt(p, i<<1));
}
void BytAtPut(word p, word i, word v) {
    PUT_BYTE(FlexAddrAt(p, i), v);
}
void PtrAtPut(word p, word i, word v) {
    PUT_WORD(FlexAddrAt(p, i<<1), v);
}

;;;;;;;;;;;;

#if 1
void Inventory() {
	fprintf(stderr, "\n");
	fprintf(stderr, "\n");
	for (word i = 0; i < 256; i++) {
		if (ClassVec[i]) {
			char buf[80];
			sprintf(buf, "ClassVec[$%02x=%3d.] = ", i, i);
			//Hex20(buf, i, ClassVec[i]);
      Inspect(ClassVec[i], buf);
			fprintf(stderr, "\n");
		}
	}
	fprintf(stderr, "\n");
	fprintf(stderr, "\n");
	for (word i = 0; i < 256; i++) {
		if (SymVec[i]) {
			char buf[80];
			sprintf(buf, "SymVec[$%02x=%3d.] = ", i, i);
			//fprintSymNum(stderr, buf, i);
      Inspect(SymVec[i], buf);
			fprintf(stderr, "\n");
		}
	}
	fprintf(stderr, "\n");
	fprintf(stderr, "\n");
  for (word i = 0; B(i); i+=2+2*B(i)) {
    Inspect(i, "OBJECT");
  }
}
#endif

bool Truth(word x) {
	return !(x==nilAddr || x==falseAddr || x==1);
}

word MakeInstance(word cls, byte flexsize) {
	Hex20("MakeInstance cls", cls, cls);
	word p = Here;
	//byte flags = B(cls + CLS_B_flags);
	//fprintf(stderr, "MakeInstance... flags=%02x\n", flags);
	word basesz = B(cls + CLS_B_numB) + 2*B(cls + CLS_B_numP);
	fprintf(stderr, "... basesz=%02x\n", basesz);
	word sz = basesz + flexsize;
	if (sz&1) sz++;  // even-align.
  if (sz < 4) sz=4;  // minimum size.
	fprintf(stderr, "... sz=%02x\n", sz);
	Here += sz;

	PUT_BYTE(p + UR_B_gcsize, (sz-2)>>1);
	PUT_BYTE(p + UR_B_cls, B(cls + CLS_B_this));
	for (byte i = 2; i < sz; i++) {
		PUT_BYTE(p+i, 0);
	}
	return p;
}

const char* FlexName(word x) {
  static char buf[30];
  word len = BytLen(x); 
  Hex20("len", len, -1);
  Hex20("flex", -1, FlexAddrAt(x, 0));
  for (word i=0; i<len; i++) {
    buf[i] = BytAt(x, i);
  }
  buf[len] = '\0';
  return buf;
}
const char* FlexNameOfClassOf(word inst) {
  Hex20("FlexNameOfClassOf", inst, inst);
  word cls = CLASSOF(inst);
  Hex20("CLASSOF", cls, cls);
  return FlexName(cls);
}

word FindMethBySymbolNumber(word rcvr, byte symNum) {
	fprintf(stderr, "FindMethBySymbolNumber: rcvr=$%04x(%s) symnum=$%02x\n", rcvr, FlexNameOfClassOf(rcvr), symNum);
	// fprintSymNum(stderr, "... want meth=", symNum);
	word cls = CLASSOF(rcvr);
	Hex20("Find Meth rcvr", rcvr, rcvr);
	Hex20("Find Meth rcvr cls", cls, cls);
	Hex20("Find Meth symNum", symNum, SymVec[symNum]);
	if (symNum >= SymVecLen) RAISE("BadSymNum");
	while (cls) {
		// Hex20("Find Meth cls", cls, cls);
		// fprintN(stderr, "... cls=", FlexAddrAt(cls, 0), BytLen(cls));
		word meth = W(cls + CLS_P_meths);
		while (meth) {
			// Hex20("Find Meth meth", meth, meth);
			// fprintSymNum(stderr, "... try meth=", B(meth + NAMED_B_name));
			if (B(meth + NAMED_B_name) == symNum) {
				return meth;
			}
			meth = W(meth + METH_P_next);
		}
		cls = W(cls + CLS_P_sup);
	}
	return RAISE("MethodNotFound");
}

byte FindSymIndex(char* s, byte len) {
  Hex20("FindSymIndex", len, 0);
  Hex20(s, len, 0);
	for (byte i = 0; i < SymVecLen; i++) {
		//fprintSymNum(stderr, ".......... try", i);
		word y = SymVec[i];
		if (B(y + SYM_B_len) != len) continue;

		word off = B(y + SYM_B_begin);
		word x = W(y + SYM_P_guts);
		word p = x + ARRBYT_FLEXSIZE + off;
		byte eq = 1;
		for (int j = 0; j < len; j++ ) {
			if (UPPER(B(p+j)) != UPPER(s[j])) {
				eq = 0;
				break;
			}
		}
		if (eq) return i;
	}
	return RAISE("FindSymIndex:SymNotFound");
}

word FindClassP(char* name, byte len) {
	Hex20("FindClassP", len, -1);
	Hex20(name, len, -1);
	for (int i = 1; i < ClassVecLen; i++) {
		word c = ClassVec[i];
		//Hex20("someclass", i, c);

    word sz = ((B(c+URCLS_B_gcsize)&127)<<1)+2;
		//Hex20("sz", sz, c+CLS_FLEXSIZE);
    word final = B(c+sz-1);
		//Hex20("final", final, c+CLS_FLEXSIZE);
    word name_len = sz - CLS_FLEXSIZE - (final==0);
		//Hex20("name_len", name_len, c+CLS_FLEXSIZE);
		if (name_len != len) continue;

		word p = c + CLS_FLEXSIZE;
		//Hex20("test-------=======", i, p);
		byte eq = 1;
		for (int j = 0; j < len; j++ ) {
			//Hex20("j--------------", j, p);
			//Hex20("left--------------", UPPER(B(p+j)), p);
			//Hex20("right--------------", UPPER(name[j]), p);
			if (UPPER(B(p+j)) != UPPER(name[j])) {
				eq = 0;
				break;
			}
		}
		if (eq) {
	    Hex20("Found....", BF(c, CLS_B_this), c);
			return c;
		}
	}
	return RAISE("ClassNotFound");
}

void LoadImage(char* filename) {
	FILE* fd = fopen(filename, "r");
	if (!fd) RAISE("CannotOpenFile");
	while (1) {
		word a = getc(fd);
		word b = getc(fd);
		word hi = getc(fd);
		word lo = getc(fd);
		word len = HL(hi,lo);
    CHECK3(b, '/', b);

		int i;
		fprintf(stderr, "A:%u B:%u len:%u\n", a, b, len);
		switch (a) {
		case 'T':
			for (i=0; i<len; i++) getc(fd);
			break;
		case 'C':
			ClassVecLen = len/2;
			for (i=0; i<ClassVecLen; i++) {
				hi = getc(fd);
				lo = getc(fd);
				ClassVec[i] = HL(hi,lo);
			}
			break;
		case 'S':
			SymVecLen = len/2;
			for (i=0; i<SymVecLen; i++) {
				hi = getc(fd);
				lo = getc(fd);
				SymVec[i] = HL(hi,lo);
			}
			break;
		case 'M':
			Here = len;
			for (i=0; i<Here; i++) {
				Memory[i] = getc(fd);
			}
			break;
		case '!':
			goto FINISH;
			break;
		default:
			RAISE("BadSegment");
			break;
		}

	}
FINISH:
	fclose(fd);
}

#if 0
void Call0(word rcvr, byte msgNum) {
	word m = FindMethBySymbolNumber(rcvr, msgNum);
	sp = 0x8000;
	fp = 0;
	pc = 0;
	PUSH(rcvr);
	PUSH(msgNum);
	PUSH(0xDE00);
	PUSH(pc);
	PUSH(fp);
	fp = sp;
	byte numL = B(m + METH_B_numL);
	for (byte i = 0; i < numL; i++) {
		PUSH(nilAddr);
	}
  // pc = FlexAddrAt(m, 0);
  Hex20("METH_FLEXSIZE", METH_FLEXSIZE, -1);
	pc = m + METH_FLEXSIZE;
	Hex20("Start Loop", pc, pc);
  Hex20("FOO", METH_FLEXSIZE, -1);
	Loop();
	Hex20("Ended Loop", pc, pc);
}
#endif

void RunDemo() {
	sp = 0x8000;
	fp = 0;
	pc = 0;

	word demo = FindClassP("DEMO", 4);
	Hex20("FindClassP demo", 888, demo);
	word inst = MakeInstance(demo, 0);
	Hex20("MakeInstance demo", 888, inst);
  Inspect(inst, "demo inst");

	byte i = FindSymIndex("RUN", 3);
	Hex20("FindSymIndex RUN", 888, i);
	word m = FindMethBySymbolNumber(inst, i);
	Hex20("FindMethBySymbolNumber RUN", 888, m);
  Inspect(m, "demo meth run");

	//pc = m + METH_FLEXSIZE + 1;
  pc = FlexAddrAt(m, 0);
  Hex20("METH_FLEXSIZE", METH_FLEXSIZE, -1);
	PUSH(0xFFFF);
	PUSH(0xDDDD);
	PUSH(0xEEEE);
	PUSH(0xCCCC);
	PUSH(inst);
	PUSH(m);
	PUSH(0xDE00);
	PUSH(pc);
	PUSH(fp);
	fp = sp;
	Hex20("Start Loop", pc, pc);
	Loop();
	Hex20("Ended Loop", pc, pc);

	//i = FindSymIndex("RUN2", 4);
	//Call0(inst, i);
}

int main() {
  CHECK3(sizeof(byte), 1, sizeof(byte));
  CHECK3(sizeof(word), 2, sizeof(word));
  InitInfo();

	LoadImage("_generated.image");
	intAddr = FindClassP("INT", 3);
	fprintf(stderr, "intAddr=%04x\n", intAddr);
  Inspect(intAddr, "intAddr");
	clsAddr = FindClassP("CLS", 3);
	fprintf(stderr, "clsAddr=%04x\n", clsAddr);
  Inspect(clsAddr, "clsAddr");

	Inventory();
	RunDemo();

	fprintf(stderr, "250 OKAY\n");
	exit(0);
}
