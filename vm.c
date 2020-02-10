#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "vm.h"
#include "_generated.h"

word Here, SymVecLen, ClassVecLen;
byte Memory[0x10000];
word SymVec[0x100];
word ClassVec[0x100];
word pc, fp, sp, tmp;

word nilAddr = 0;
word falseAddr = 2;
word trueAddr = 4;
word intClassAddr;

word here;
word gcroot;
word clsroot;

void Hex20(char* s, int d, word p) {
	fprintf(stderr, "[%s $%04x=%d.] %04x: ", s, d, d, p);
	for (int i = 0; i < 20; i++) {
		byte b = B(p+i);
		fprintf(stderr, "%02x ", b);
		if ((i&3)==3) fprintf(stderr, " ");
	}
	for (int i = 0; i < 20; i++) {
		byte b = B(p+i);
		fprintf(stderr, "%c", (' '<=b && b<='~' ? b : '.'));
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

	Hex20("(sym)", symNum, sym);
	Hex20("(guts)", symNum, guts);
	fprintN(fd, message, guts + ARRBYT_FLEXSIZE + begin, len);
}

void Inventory() {
	fprintf(stderr, "\n");
	fprintf(stderr, "\n");
	for (word i = 0; i < 256; i++) {
		if (ClassVec[i]) {
			char buf[80];
			sprintf(buf, "ClassVec[$%02x=%3d.] = ", i, i);
			Hex20(buf, i, ClassVec[i]);
			fprintf(stderr, "\n");
		}
	}
	fprintf(stderr, "\n");
	fprintf(stderr, "\n");
	for (word i = 0; i < 256; i++) {
		if (SymVec[i]) {
			char buf[80];
			sprintf(buf, "SymVec[$%02x=%3d.] = ", i, i);
			fprintSymNum(stderr, buf, i);
			fprintf(stderr, "\n");
		}
	}
	fprintf(stderr, "\n");
	fprintf(stderr, "\n");
}

bool Truth(word x) {
	return !(x==nilAddr || x==falseAddr || x==1);
}

word MakeInstance(word cls, word flexbytes, byte flexsize) {
	Hex20("MakeInstance cls", cls, cls);
	word p = Here;
	byte flags = B(cls + CLS_B_flags);
	fprintf(stderr, "MakeInstance... flags=%02x", flags);
	byte basesz = B(cls + CLS_B_numB) + 2*B(cls + CLS_B_numP);
	fprintf(stderr, "MakeInstance... basesz=%02x", basesz);
	byte sz = basesz;
	if (flexbytes) {
		sz += flexsize;  // add flex if needed.
		if(flags == 0) RAISE("ShouldHaveFlexFlag");
	} else {
		if(flags != 0) RAISE("ShouldNotHaveFlexFlag");
	}
	if (sz&1) sz++;  // even-align.
	fprintf(stderr, "MakeInstance... sz=%02x", sz);
	Here += sz;

	PUT_BYTE(p + UR_B_gcsize, sz>>1);
	PUT_BYTE(p + UR_B_cls, B(cls + CLS_B_this));
	for (byte i = 2; i < basesz; i++) {
		PUT_BYTE(p+i, 0);
	}
	if (flexbytes) {
		PUT_BYTE(p + basesz, flexsize);
		if (flexbytes != 0xFFFF) {
			for (byte i = 0; i < flexsize; i++) {
				PUT_BYTE(p + basesz + 1 + i, B(flexbytes+i));
			}
		}
	}
	return p;
}

word FindMethBySymbolNumber(word rcvr, byte symNum) {
	fprintf(stderr, "FindMethBySymbolNumber: %04x %02x\n", rcvr, symNum);
	fprintSymNum(stderr, "... want meth=", symNum);
	word cls;
	if (rcvr & 1) {
		// Tagged short int.
		cls = intClassAddr;
	} else {
		byte b = B(rcvr + UR_B_cls);
		if (b >= ClassVecLen) RAISE("BadClsSlot");
		cls = ClassVec[B(rcvr + UR_B_cls)];
	}
	Hex20("Find Meth rcvr", rcvr, rcvr);
	Hex20("Find Meth symNum", symNum, SymVec[symNum]);
	if (symNum >= SymVecLen) RAISE("BadSymNum");
	while (cls) {
		Hex20("Find Meth cls", cls, cls);
		fprintN(stderr, "... cls=", cls + CLS_FLEXSIZE + 1, B(cls + CLS_FLEXSIZE));
		word meth = W(cls + CLS_P_meths);
		while (meth) {
			Hex20("Find Meth meth", meth, meth);
			fprintSymNum(stderr, "... try meth=", B(meth + NAMED_B_name));
			if (B(meth + NAMED_B_name) == symNum) {
				return meth;
			}
			meth = W(meth + METH_P_next);
		}
		cls = W(cls + CLS_P_sup);
	}
	RAISE("MethodNotFound");
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
	RAISE("FindSymIndex:SymNotFound");
}

word FindClassP(char* name, byte len) {
	Hex20("FindClassP", len, 0);
	Hex20(name, len, 0);
	for (int i = 1; i < ClassVecLen; i++) {
		word c = ClassVec[i];
		Hex20("someclass", i, c);

    word sz = ((B(c+URCLS_B_gcsize)&127)<<1)+2;
		Hex20("sz", sz, c+CLS_FLEXSIZE);
    word final = B(c+sz-1);
		Hex20("final", final, c+CLS_FLEXSIZE);
    word name_len = sz - CLS_FLEXSIZE - (final==0);
		Hex20("name_len", name_len, c+CLS_FLEXSIZE);
		if (name_len != len) continue;

		word p = c + CLS_FLEXSIZE;
		Hex20("test-------=======", i, p);
		byte eq = 1;
		for (int j = 0; j < len; j++ ) {
			Hex20("j--------------", j, p);
			Hex20("left--------------", UPPER(B(p+j)), p);
			Hex20("right--------------", UPPER(name[j]), p);
			if (UPPER(B(p+j)) != UPPER(name[j])) {
				eq = 0;
				break;
			}
		}
		if (eq) {
			return c;
		}
	}
	RAISE("ClassNotFound");
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
	pc = m + METH_FLEXSIZE + 1;
	Hex20("Start Loop", pc, pc);
	Loop();
	Hex20("Ended Loop", pc, pc);
}

void RunDemo() {
	sp = 0x8000;
	fp = 0;
	pc = 0;

	word demo = FindClassP("DEMO", 4);
	Hex20("FindClassP demo", 888, demo);
	word inst = MakeInstance(demo, 0, 0);
	Hex20("MakeInstance demo", 888, inst);

	byte i = FindSymIndex("RUN", 3);
	Hex20("FindSymIndex RUN", 888, i);
	word m = FindMethBySymbolNumber(inst, i);
	Hex20("FindMethBySymbolNumber RUN", 888, m);

	pc = m + METH_FLEXSIZE + 1;
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

	i = FindSymIndex("RUN2", 4);
	Call0(inst, i);
}

int main() {
  CHECK3(sizeof(byte), 1, sizeof(byte));
  CHECK3(sizeof(word), 2, sizeof(word));

	LoadImage("_generated.image");
	intClassAddr = FindClassP("INT", 3);
	fprintf(stderr, "intClassAddr=%04x\n", intClassAddr);

	Inventory();
	RunDemo();

	fprintf(stderr, "250 OKAY\n");
	exit(0);
	Loop();
}
