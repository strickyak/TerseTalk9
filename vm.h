#ifndef TERSETALK9_VM_H_
#define TERSETALK9_VM_H_

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define CHECK3(X,Y,Z) { word _x = (X); word _y = (Y); if(_x!=_y) { fprintf(stderr, "CHECK FAILS: file %s line %d: 0x%04x != 0x%04x: extra=0x%04x\n", __FILE__, __LINE__, (int)_x, (int)_y, (int)(Z)); } }

typedef unsigned char bool;
typedef unsigned char byte;
typedef unsigned short word;

extern byte Memory[];
extern word SymVec[];
extern word ClassVec[];

extern word Intern[256];     // Symbols.  Index is message number.

extern word nilAddr, falseAddr, trueAddr;
extern word intClassAddr;

extern word pc, fp, sp, tmp;

#define BYTE(A) (*(byte*)(Memory+(A)))
#define B(A) (*(byte*)(Memory+(A)))
#define PUT_BYTE(A,X) ((*(byte*)(Memory+(A))) = (X))

#define WORD(A) ( ((word)(B(A)) << 8) | (word)(B((A)+1)) )
#define W(A) ( ((word)(B(A)) << 8) | (word)(B((A)+1)) )
#define PUT_WORD(A,X) (Memory[(word)(A)] = 255&(((word)(X)) >> 8), Memory[(word)(A)+1] = 255&((word)(X)))

#define PEEK(N) WORD(sp + (N))
#define POKE(N,X) PUT_WORD(sp + (N), X)
#define POP() ((tmp = WORD(sp)), (sp+=2), tmp)
#define PUSH(X) ((sp-=2), PUT_WORD(sp, (X)))

#define LOCAL(N) WORD(fp + 2 * (4 + (N)))  // over prevFP, prevPC, numArg, msg.  rcvr=0, 1,2,3=arg1,2,3
#define PUT_LOCAL(N,X) PUT_WORD(fp + 2 * (4 + (N)), (X))

extern word MakeInstance(word cls, word flexbytes, byte flexsize);
extern word FindMethBySymbolNumber(word rcvr, byte msg);
extern byte FindSymIndex(char* s, byte len);
extern word FindClassP(char* name, byte len);
extern void Loop();
extern void Hex20(char* label, int d, word p);
// NIL, FALSE, and 0 are false; all else are true.
extern bool Truth(word x);

#define RAISE(S) {fprintf(stderr, "\n*** RAISE: %s\n", (S)); exit(13);}
#define HL(H,L) ((((word)(H)&255)<<8) | ((word)(L)&255))
#define UPPER(C) ('a' <= (byte)(C) && (byte)(C) <= 'z' ? (byte)(C)-32 : (byte)(C))

#define UB2OOP(B) (((word)(B)<<1)|1)  // Unsigned byte to oop.
#define OOP2UB(P) ((byte)((word)(P)>>1))

#define CLASSOF(X) ((X)&1 ? intClassAddr : ClassVec[B((X) + CLS_B_cls)])

#endif
