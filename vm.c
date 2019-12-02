#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "vm.h"
#include "_generated.h"

word MemoryLen, SymVecLen, ClassVecLen;
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
	fprintf(stderr, "Hex20(%s#%04x=%d.)%04x: ", s, d, d, p);
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

word MakeInstance(word cls, word flexbytes, byte flexsize) {
	word p = MemoryLen;
	byte flags = B(cls + CLASS_B_flags);
	byte basesz = B(cls + CLASS_B_numB) + 2*B(cls + CLASS_B_numP);
	byte sz = basesz;
	if (flexbytes) {
		sz += 1 + flexsize;  // add flex if needed.
		if(flags == 0) RAISE("ShouldHaveFlexFlag");
	} else {
		if(flags != 0) RAISE("ShouldNotHaveFlexFlag");
	}
	if (sz&1) sz++;  // even-align.
	MemoryLen += sz;

	PUT_BYTE(p + UR_B_gcsize, sz>>1);
	PUT_BYTE(p + UR_B_cls, B(cls + CLASS_B_this));
	for (byte i = 2; i < basesz; i++) {
		PUT_BYTE(p+i, 0);
	}
	if (flexbytes) {
		PUT_BYTE(p + basesz, flexsize);
		for (byte i = 0; i < flexsize; i++) {
			PUT_BYTE(p + basesz + 1 + i, B(flexbytes+i));
		}
	}
	return p;
}

word FindMethBySymbolNumber(word rcvr, byte symNum) {
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
		word meth = W(cls + CLASS_P_meths);
		while (meth) {
			Hex20("Find Meth meth", meth, meth);
			if (B(meth + NAMED_B_name) == symNum) {
				return meth;
			}
			meth = W(meth + METH_P_next);
		}
		cls = W(cls + CLASS_P_sup);
	}
	RAISE("MethodNotFound");
}

byte FindSymIndex(char* s, byte len) {
	byte n = strlen(s);
	for (int i = 0; i < SymVecLen; i++) {
		word y = SymVec[i];
		if (B(y + SYM_B_len) != n) continue;

		word off = B(y + SYM_B_begin);
		word x = W(y + SYM_P_guts);
		word p = x + FLEXB_FLEXSIZE + 1 + off;
		for (int j = 0; j < n; j++ ) {
			if (UPPER(B(p)) != UPPER(s[j])) continue;
		}
		return i;
	}
	RAISE("SymNotFound");
}

word FindClassP(char* name, byte len) {
	for (int i = 1; i < ClassVecLen; i++) {
		word c = ClassVec[i];
		Hex20("someclass", i, c);
		Hex20("flexsize", CLASS_FLEXSIZE, c+CLASS_FLEXSIZE);
		if (B(c + CLASS_FLEXSIZE) != len) continue;
		word p = c + CLASS_FLEXSIZE + 1;
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
		if (eq) return c;
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
			MemoryLen = len;
			for (i=0; i<MemoryLen; i++) {
				Memory[i] = getc(fd);
			}
			break;
		case 0:
			goto STOP;
			break;
		default:
			RAISE("BadSegment");
			break;
		}

	}
STOP:
	fclose(fd);
}

void RunDemo() {
	sp = 0x8000;
	fp = 0;
	pc = 0;
	word demo = FindClassP("DEMO", 4);
	Hex20("FindClassP demo", 888, demo);
	word inst = MakeInstance(demo, 0, 0);
	Hex20("MakeInstance demo", 888, inst);
	byte msgI = FindSymIndex("RUN", 3);
	Hex20("FindSymIndex RUN", 888, msgI);
	word m = FindMethBySymbolNumber(inst, msgI);
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
}

int main() {
	LoadImage("_generated.image");
	intClassAddr = FindClassP("INT", 3);
	RunDemo();
	fprintf(stderr, "250 OKAY\n");
	exit(0);
	Loop();
}
