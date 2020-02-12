#ifndef TERSETALK9_VM_H_
#define TERSETALK9_VM_H_

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define CHECK3(X,Y,Z) { word _x = (X); word _y = (Y); if(_x!=_y) { fprintf(stderr, "CHECK FAILS: file %s line %d: 0x%04x != 0x%04x: extra=0x%04x\n", __FILE__, __LINE__, (int)_x, (int)_y, (int)(Z)); abort(); } }

typedef unsigned char bool;
typedef unsigned char byte;
typedef unsigned short word;

extern byte Memory[];
extern word SymVec[];
extern word ClassVec[];

extern word Intern[256];     // Symbols.  Index is message number.

extern word nilAddr, falseAddr, trueAddr;
extern word intAddr, clsAddr;

extern word pc, fp, sp, tmp;

#define BYTE(A) (*(byte*)(Memory+(A)))
#define B(A) (*(byte*)(Memory+(A)))
#define PUT_BYTE(A,X) ((*(byte*)(Memory+(A))) = (X))

#define WORD(A) ( ((word)(B(A)) << 8) | (word)(B((A)+1)) )
#define W(A) ( ((word)(B(A)) << 8) | (word)(B((A)+1)) )
#define PUT_WORD(A,X) (Memory[(word)(A)] = 255&(((word)(X)) >> 8), Memory[(word)(A)+1] = 255&((word)(X)))

#define PEEK(N) WORD(sp + (N))
#define POKE(N,X) PUT_WORD(sp + (N), X)
#define PUSH(X) ((sp-=2), PUT_WORD(sp, (X)))
#define POP() ((tmp = WORD(sp)), (sp+=2), tmp)
#define DROP() (sp+=2)

#define LOCAL(N) WORD(fp + 2 * (4 + (N)))  // over prevFP, prevPC, numArg, msg.  rcvr=0, 1,2,3=arg1,2,3
#define PUT_LOCAL(N,X) PUT_WORD(fp + 2 * (4 + (N)), (X))

word MakeInstance(word cls, byte flexsize);
word FindMethBySymbolNumber(word rcvr, byte msg);
byte FindSymIndex(char* s, byte len);
word FindClassP(char* name, byte len);
void Loop();
void Hex20(const char* label, int d, word p);
// NIL, FALSE, and 0 are false; all else are true.
bool Truth(word x);

word FlexAddrAt(word p, word i);
word BytLen(word p);
word PtrLen(word p);
byte BytAt(word p, word i);
word PtrAt(word p, word i);
void BytAtPut(word p, word i, word v);
void PtrAtPut(word p, word i, word v);
const char* FlexName(word cls);
const char* FlexNameOfClassOf(word inst);
int RAISE(const char* s);

#define HL(H,L) ((((word)(H)&255)<<8) | ((word)(L)&255))
#define UPPER(C) ('a' <= (byte)(C) && (byte)(C) <= 'z' ? (byte)(C)-32 : (byte)(C))

#define NUM2OOP(N) (((word)(N)<<1)|1)
#define OOP2NUM(P) ((word)(P)>>1)
#define OOP2BYTE(P) (byte)((word)(P)>>1)

#define BF(P,F) B((P)+(F))  // get byte-sized field
#define WF(P,F) W((P)+(F))  // get word-sized field
#define CLASSOF(X) ((X)&1 ? intAddr : ClassVec[B((X) + CLS_B_cls)])
#define BASE_SiZE(X) ((X)&1 ? 0 : ClassVec[B((X) + CLS_B_cls)])

struct FieldInfo {
  const char* name;
  byte width;
  byte offset;
};

struct ClassInfo {
  const char* name;
  byte num;
  struct FieldInfo* fields;
};

extern struct ClassInfo* ClassInfos[256];
extern void InitInfo();
extern void Inspect(word w, const char* msg);
extern void PrintSymNum(byte num, const char* label);
extern void PrintStr(word str, const char* label);
#endif
