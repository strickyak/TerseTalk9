# TODO: put the name flexstring in the Class. 
# Class is not "Named" and its names are not interned.
# Sym continues like today.  Meth is named.

import os, re, sys
import collections
from logging import info

# Tuning.
MEMORY_LEN = 0x8000  # Somewhat arbtrary.
SYM_VEC_LEN = 256
CLASS_VEC_LEN = 256

# Leave a little gap for future overhead.
MAX_OBJ_SIZE = 258
MAX_FLEX_BYTES = 256
MAX_FLEX_PTRS = 128

# Target memory.
Memory = MEMORY_LEN * ['#']
SymVec = SYM_VEC_LEN * [0]
ClassVec = CLASS_VEC_LEN * [0]

# Compiler state.
OpList = []
OpNums = {}
Method = collections.defaultdict(dict)
Op = {}

# Util
def Hi(x): return 255 & (x>>8)
def Lo(x): return 255 & x

##### LEX

LEX_INT = re.compile('(-?[0-9]+|[$][0-9a-fA-F]+)').match
LEX_COLON = re.compile('(([A-Za-z][A-Za-z0-9]*)\\s*[:])').match
LEX_IDENT = re.compile('([A-Za-z][A-Za-z0-9]*)').match
LEX_MULOP = re.compile('([*]|/|%)').match
LEX_ADDOP = re.compile('([+]|-)').match
LEX_RELOP = re.compile('(<|>|==|!=|<=|>=)').match
LEX_PUNCT = re.compile('([():,.;=])').match
LEX_WHITE = re.compile('([ \\t\\n\\r]*)').match

PATTERNS = [('C',LEX_COLON), ('I',LEX_INT), ('W',LEX_IDENT), ('M',LEX_MULOP), ('A',LEX_ADDOP), ('R',LEX_RELOP), ('P',LEX_PUNCT)]

class Lex(object):
    def __init__(self, source):
        self.source = source
        self.n = len(source)
        self.i = 0
        print ('Inital', self.source, self.i, self.n)
        self.Advance()

    def Advance(self):
        self.token = self.Next()

    def Next(self):
        if self.i == self.n:
            print 'Next', 60, ('Z', '', self.i)
            return ('Z', '', self.i)

        rest = self.source[self.i:]
        white = LEX_WHITE(rest)
        if white:
            self.i += len(white.group(1))

        if self.i == self.n:
            print 'Next', 69, ('Z', '', self.i)
            return ('Z', '', self.i)

        rest = self.source[self.i:]
        for typ,pat in PATTERNS:
            m = pat(rest)
            if m:
                self.i += len(m.group(1))
                print 'Next', 78, (typ, m.group(1), self.i)
                return (typ, m.group(1), self.i)
        raise Exception('Cannot lex rest: %s' % repr(rest))

class PExpr(object):
    pass
class PSeq(PExpr):
    def __init__(self, exprs):
        self.exprs = exprs
    def __str__(self):
        return '{%s}' % ' ; '.join(str(e) for e in self.exprs)
    def visit(self, v):
        v.visitSeq(self)
class PAssign(PExpr):
    def __init__(self, varz, expr):
        self.vars = varz
        self.expr = expr
    def __str__(self):
        return '%s= %s' % (self.vars, self.expr)
    def visit(self, v):
        v.visitAssign(self)
class PList(PExpr):
    def __init__(self, exprs):
        self.exprs = exprs
    def __str__(self):
        return '[%s]' % ' , '.join(str(e) for e in self.exprs)
    def visit(self, v):
        v.visitList(self)
class PVar(PExpr):
    def __init__(self, s):
        self.s = s.upper()
    def __str__(self):
        return '%s' % self.s
    def visit(self, v):
        v.visitVar(self)
class PInt(PExpr):
    def __init__(self, n):
        self.n = n
    def __str__(self):
        return '%d' % self.n
    def visit(self, v):
        v.visitInt(self)
class PUnary(PExpr):
    def __init__(self, r, meth):
        self.r = r
        self.meth = meth.upper()
    def __str__(self):
        return '%s %s' % (self.r, self.meth)
    def visit(self, v):
        v.visitUnary(self)
class PMul(PExpr):
    def __init__(self, r, meth, a):
        self.r = r
        self.meth = meth
        self.a = a
    def __str__(self):
        return '(%s %s %s)' % (self.r, self.meth, self.a)
    def visit(self, v):
        v.visitMul(self)
class PAdd(PExpr):
    def __init__(self, r, meth, a):
        self.r = r
        self.meth = meth
        self.a = a
    def __str__(self):
        return '(%s %s %s)' % (self.r, self.meth, self.a)
    def visit(self, v):
        v.visitAdd(self)
class PRel(PExpr):
    def __init__(self, r, meth, a):
        self.r = r
        self.meth = meth
        self.a = a
    def __str__(self):
        return '(%s %s %s)' % (self.r, self.meth, self.a)
    def visit(self, v):
        v.visitRel(self)
class PKeyword(PExpr):
    def __init__(self, r, meth, args):
        self.r = r
        self.meth = meth.upper()
        self.args = args
    def __str__(self):
        z = '( (%s) ' % self.r
        for k,v in zip(self.meth.split(':'),  [str(a) for a in self.args]):
            z += '%s: %s ' % (k, v)
        return z + ')'
    def visit(self, v):
        v.visitKeyword(self)
class PMacro(PExpr):
    def __init__(self, keywords, varz, exprs):
        self.keywords = keywords
        self.vars = varz
        self.exprs = exprs
    def __str__(self):
        z = ''
        for k,v,e in zip(self.keywords, self.vars, self.exprs):
            z += '%s(%s%s)' % (k, ('%s:' % v if v else ''), e)
        return z
    def visit(self, v):
        v.visitMacro(self)

class Parser(object):
    def __init__(self, source):
        self.source = source
        self.lex = Lex(source)

    def Parse(self):
        seq = self.ParseSeq()
        typ, s, i = self.lex.token
        if typ != 'Z':
            print('Extra stuff: %s' % repr((typ, s, i)))
            raise Exception('Extra stuff: %s' % repr(self.source[i:]))
        return seq

    def ParseSeq(self):
        z = []
        while True:
            a = self.ParseAssign()
            z.append(a)
            typ, s, i = self.lex.token
            if typ=='Z' or s==')' or s=='=':
                break
            elif s=='.' or s==';':
                self.lex.Advance()
                typ, s, i = self.lex.token
                if typ=='Z' or s==')' or s=='=':  # If trailing "."
                    break
            else:
                raise Exception('EXPECTED EOS or ")" or "." or ";" AFTER %s BEFORE %s' % (repr(self.source[:i]), repr(self.source[i:])))
        return z[0] if len(z)==1 else PSeq(z)

    def ParseAssign(self):
        a = self.ParseList()
        typ, s, i = self.lex.token
        if s == '=':
            if not isinstance(a, PList) and not isinstance(a, PVar):
                raise Exception('Bad target of assignment AFTER %s BEFORE %s' % (repr(self.source[:i]), repr(self.source[i:])))
            if isinstance(a, PList):
                for b in a.exprs:
                    if not isinstance(b, PVar):
                        raise Exception('Bad subtarget "%s" of assignment AFTER %s BEFORE %s' % (b, repr(self.source[:i]), repr(self.source[i:])))
            self.lex.Advance()
            b = self.ParseList()
            return PAssign(a, b)
        return a

    def ParseList(self):
        z = []
        while True:
            a = self.ParseKeyword()
            z.append(a)
            typ, s, i = self.lex.token
            if s==',':
                self.lex.Advance()
            else:
                break
        return z[0] if len(z)==1 else PList(z)

    def ParseMacro(self, name):
        typ, s, i = self.lex.token
        keywords = [name]
        varz = []
        exprs = []
        while True:
            if varz: # Not the first time:
                keywords.append(s)

                # next comes the open paren
                self.lex.Advance()
                typ, s, i = self.lex.token
                if s != '(':
                    raise Exception('Expected "(" in macro AFTER %s BEFORE %s' % (repr(self.source[:i]), repr(self.source[i:])))
                self.lex.Advance()
                typ, s, i = self.lex.token

            var = None
            if typ == 'C':
                var = LEX_COLON(s).group(2)  # extract word.
                self.lex.Advance()

            ex = self.ParseSeq()

            varz.append(var)
            exprs.append(ex)

            typ, s, i = self.lex.token
            if s != ')':
                raise Exception('Expected ")" in macro AFTER %s BEFORE %s' % (repr(self.source[:i]), repr(self.source[i:])))

            self.lex.Advance()
            typ, s, i = self.lex.token
            if typ != 'W':
                break
        return PMacro(keywords, varz, exprs)

    def ParsePrim(self):
        typ, s, i = self.lex.token
        if typ == 'I':
            self.lex.Advance()
            val = int(s[1:],base=16) if s[0]=='$' else int(s)
            return PInt(val)
        elif typ == 'W':
            name = s
            self.lex.Advance()
            typ, s, i = self.lex.token
            if s == '(':
                # Macro syntax
                self.lex.Advance()
                return self.ParseMacro(name)
            else:
                # Just a var name
                return PVar(name)
        elif s == '(':
            self.lex.Advance()
            seq = self.ParseSeq()
            typ, s, i = self.lex.token
            if s != ')':
                raise Exception('EXPECTED ")" AFTER %s BEFORE %s' % (repr(self.source[:i]), repr(self.source[i:])))
            self.lex.Advance()
            return seq
        else:
            raise Exception('UNEXPECTED prim: %s' % repr((typ, s, i)))

    def ParseKeyword(self):
        rargs = [ self.ParseRel() ]  # rargs are receiver and args.
        keywords = ''
        while True:
            typ, s, i = self.lex.token

            if typ == 'C':  # a word and a colon
                s = LEX_COLON(s).group(2)  # extract word.
                keywords += s + ':'
                self.lex.Advance()
                rargs.append(self.ParseRel())

            else:
                break

        if len(rargs) > 1:
            return PKeyword(rargs[0], keywords, rargs[1:])
        else:        
            return rargs[0]

    def ParseRel(self):
        a = self.ParseAdd()

        typ, s, i = self.lex.token
        if typ == 'R':
            op = s
            self.lex.Advance()
            b = self.ParseAdd()
            return PRel(a, op, b)
        return a

    def ParseAdd(self):
        a = self.ParseMul()

        typ, s, i = self.lex.token
        if typ == 'A':
            op = s
            self.lex.Advance()
            b = self.ParseMul()
            return PAdd(a, op, b)
        return a

    def ParseMul(self):
        a = self.ParseUnary()

        typ, s, i = self.lex.token
        if typ == 'M':
            op = s
            self.lex.Advance()
            b = self.ParseUnary()
            return PMul(a, op, b)
        return a

    def ParseUnary(self):
        a = self.ParsePrim()

        typ, s, i = self.lex.token
        while typ == 'W':
            a = PUnary(a, s)
            self.lex.Advance()
            typ, s, i = self.lex.token
        return a
            
for s in [
    '4',
    '4 - 6',
    'sys print:( x square + y square ) sqrt ',
    'sys print:( x square + y square ) sqrt on: stdout',
    '2. 4. 6. 64 sqrt',
    '(foo foo; bar bar; 2, 4, 6, 64 sqrt) len',
    '(1,2,3),(4,5,6),(7,8,9)',
    '(1,2,3)x,(4,5,6)y,(7,8,9)z',
    'r, s, t = (1,2,3)x,(4,5,6)y,(7,8,9)z',
    'z = IF(a<2)THEN(a+0)ELSE(demo recurse: a - 1)',
    'a,b,c = FOR(i: words)MAP( IF(i<0)THEN(i neg) ELSE (i) )',
    ]:
        print '<<< %s' % s
        print '>>> %s' % str(Parser(s).Parse())


class LocalsVisitor(object):
    def __init__(self):
        self.locals = set()
    def visitSeq(self, p):
        for e in p.exprs:
            e.visit(self)
    def visitAssign(self, p):
        p.expr.visit(self)
        if isinstance(p.vars, list):
            for v in p.vars:
                self.locals.add(v.s)
        elif isinstance(p.vars, PVar):
            self.locals.add(p.vars.s)
        else:
            raise type(p)
    def visitList(self, p):
        for e in p.exprs:
            e.visit(self)
    def visitVar(self, p):
        pass
    def visitInt(self, p):
        pass
    def visitUnary(self, p):
        p.r.visit(self)
    def visitMul(self, p):
        p.r.visit(self)
        p.a.visit(self)
    def visitAdd(self, p):
        p.r.visit(self)
        p.a.visit(self)
    def visitRel(self, p):
        p.r.visit(self)
        p.a.visit(self)
    def visitKeyword(self, p):
        p.r.visit(self)
        for e in p.args:
            e.visit(self)
    def visitMacro(self, p):
        for v in p.vars:
            self.locals.add(v)
        for e in p.exprs:
            e.visit(self)

class CompilerVisitor(object):
    def __init__(self, top, cls):
        self.top = top
        self.cls = cls
        self.explain = []
        self.codes = []
        self.slots = {}
        self.flex = None
        self.localindex = {}

        for k,offset in self.cls.bslots:
            # Strip prefix b_ from k.
            self.slots[k[2:].upper()] = ('b', offset)

        for k,offset in self.cls.pslots:
            # Strip prefix p_ from k.
            self.slots[k[2:].upper()] = ('p', offset)

        for k,offset in self.cls.flexes:
            # Like ('FLEX_BYTES', 2) or ('FLEX_PTRS', 2).
            self.flex = (k, offset)

        # Find all names assigned.  Filter out the slots, to get locals.
        # (This is like the python rule: instead of declaring locals, assign them.)
        lv = LocalsVisitor()
        top.visit(lv)
        self.locals = sorted([e for e in lv.locals if e not in self.slots])

        for i,var in zip(range(len(self.locals)), self.locals):
            self.localindex[var] = i

    def AddLocal(self, var):
        i = len(self.locals)
        self.localindex[var] = i
        self.locals.append(var)
        return i

    def visitSeq(self, p):
        last = p.exprs.pop()
        for e in p.exprs:
            e.visit(self)
            self.codes.append('drop')  # Drop middle results.
        last.visit(self)  # the last one returns the result.

    def visitAssign(self, p):
        p.expr.visit(self)
        self.codes.append('dup')  # one to assign and one for result.

        if isinstance(p.vars, list):
            raise 'TODO: list deconstruction'
        var = p.vars.s

        print 'visitAssign:', p, '|', p.vars, '|', p.expr
        print 'self.slots,var:', self.slots, '|', var, type(var)
        slot = self.slots.get(var)
        if slot:
            kind, offset = slot
            if kind=='b':
                self.codes.append('self')
                self.codes.append('putb_b')
                self.codes.append(offset)
            elif kind=='p':
                self.codes.append('self')
                self.codes.append('putp_b')
                self.codes.append(offset)
            else:
                raise 'bad'
        else:
            # Not a slot, so it should be a local var.
            i = self.localindex[var]
            if i<4 and False:
                self.codes.append('sto%d' % i)
            else:
                self.codes.append('sto_b')
                self.codes.append(i)
        
    def visitList(self, p):
        raise 'TODO'

    def visitVar(self, p):
        var = p.s
        slot = self.slots.get(var)
        cls = ClassDict.get(var)
        if var in ['SELF','SUPER','TRUE','FALSE','NIL','A','B','C','D']:
            self.codes.append(var)
        elif slot:
            kind, offset = slot
            if kind=='b':
                self.codes.append('self')
                self.codes.append('getb_b')
                self.codes.append(offset)
            elif kind=='p':
                self.codes.append('self')
                self.codes.append('getp_b')
                self.codes.append(offset)
            else:
                raise 'bad'
        elif cls:
            self.codes.append('cls_b')
            self.codes.append(cls.b_this)
        else:
            i = self.localindex[var]
            if i<4 and False:
                self.codes.append('rcl%d' % i)
            else:
                self.codes.append('rcl_b')
                self.codes.append(i)
    def visitInt(self, p):
        n = p.n
        if -64 <= n < 64:
            if n<0: n+=256
            self.codes.append('lit_b')
            self.codes.append(255&((n<<1)|1))
        else:
            if n<0: n+=65536
            self.codes.append('lit_w')
            self.codes.append(255&(n>>7))
            self.codes.append(255&((n<<1)|1))
    def visitUnary(self, p):
        p.r.visit(self)
        self.codes.append('call0_b')
        self.codes.append(InternDict[p.meth])
    def visitMul(self, p):
        p.a.visit(self)
        p.r.visit(self)
        self.codes.append('call1_b')
        self.codes.append(InternDict[p.meth])
    def visitAdd(self, p):
        p.a.visit(self)
        p.r.visit(self)
        self.codes.append('call1_b')
        self.codes.append(InternDict[p.meth])
    def visitRel(self, p):
        p.a.visit(self)
        p.r.visit(self)
        self.codes.append('call1_b')
        self.codes.append(InternDict[p.meth])
    def visitKeyword(self, p):
        args = p.args[:]
        args.reverse()
        for a in args:
            a.visit(self)
        p.r.visit(self)
        self.codes.append('call%d_b' % len(args))
        self.codes.append(InternDict[p.meth])
    def visitMacro(self, p):
        name = '_'.join(p.keywords)
        macro = MACROS[name]
        macro(self, p.vars, p.exprs)

_Serial = 0
def Serial():
    global _Serial
    _Serial += 1
    return _Serial

def IfThenMacro(v, varz, exprs):
    varz.append(None)
    exprs.append(PVar('NIL'))
    IfThenElseMacro(v, varz, exprs)

def IfThenElseMacro(v, varz, exprs):
    assert all([var is None for var in varz])
    mark1 = Serial()
    mark2 = Serial()
    exprs[0].visit(v)
    v.codes.append('/bfalse/%d' % mark1)
    exprs[1].visit(v)
    v.codes.append('/jump/%d' % mark2)
    v.codes.append('/mark/%d' % mark1)
    exprs[2].visit(v)
    v.codes.append('/mark/%d' % mark2)

def WhileDoMacro(v, varz, exprs):
    assert all([var is None for var in varz])
    mark1 = Serial()
    mark2 = Serial()
    v.codes.append('/mark/%d' % mark1)
    exprs[0].visit(v)
    v.codes.append('/bfalse/%d' % mark2)
    exprs[1].visit(v)
    v.codes.append('/jump/%d' % mark1)
    v.codes.append('/mark/%d' % mark2)

def ForDoMacro(v, varz, exprs):
    assert varz[0]
    assert not varz[1]

    # Create the local variable for Limit:
    limit = '_tmp_%d' % Serial()
    lim = v.AddLocal(limit.upper())
    # Find the index variable.
    ix = v.AddLocal(varz[0].upper())

    # Store 0 in index.
    v.codes.append('lit_b')
    v.codes.append('1')
    v.codes.append('sto_b')
    v.codes.append(ix)

    # Evaluate limit.
    exprs[0].visit(v)
    v.codes.append('sto_b')
    v.codes.append(lim)

    mark1 = Serial()
    mark2 = Serial()
    v.codes.append('/mark/%d' % mark1)

    # Check for ix reached the limit.
    v.codes.append('rcl_b')
    v.codes.append(lim)
    v.codes.append('rcl_b')
    v.codes.append(ix)
    v.codes.append('subtract')

    v.codes.append('/bfalse/%d' % mark2)
    exprs[1].visit(v)
    v.codes.append('incr_local_b')
    v.codes.append(ix)
    v.codes.append('/jump/%d' % mark1)
    v.codes.append('/mark/%d' % mark2)


MACROS = dict(
        IF_THEN = IfThenMacro,
        IF_THEN_ELSE = IfThenElseMacro,
        WHILE_DO = WhileDoMacro,
        FOR_DO = ForDoMacro,
        )

def CompileToCodes(s, cls):
    p = Parser(s).Parse()
    v = CompilerVisitor(p, cls)
    p.visit(v)
    return v.codes, len(v.locals)

InternDict = {}  # str to index.
def Intern(s):
    n = InternDict.get(s)
    if not n:
        n = len(InternDict)
        InternDict[s] = n
    return n
Intern("")  # Empty string is intern index 0.

CLASS_PATTERN = re.compile("^@([A-Za-z0-9_:]+)$").match
SYM_PATTERN = re.compile("^#([A-Za-z0-9_:]+)$").match
INT_PATTERN = re.compile("^-?[0-9]+$").match
MARK_PATTERN = re.compile("^/([a-z]+)/([0-9]+)$").match

def EvalInt(s):
    z = 0
    for ch in s:
        i = ord(ch) - ord('0')
        if 0 <= i and i <= 9:
            z = 10*z + i
        else:
            raise Exception('Bad decimal digit in string: %s' % s)
    return z

def Num2Oop(x):
    z = (x << 1) | 1
    if z > 0xFFFF:
        raise Exception('Num2Oop too big: %d.' % x)
    return z

# Nicknames are just for debugging the compiler.
Nick = 0
def GetNick():
    global Nick
    Nick+=1
    return Nick

# All objects that need copying into Mem.
Here = 0
MemorableList = []

class Memorable(object):
    def __init__(self):
        self.nick = GetNick()

    def Reify(self):
        global Here
        assert len(getattr(self, 'flexbytes', [])) == getattr(self, 'flexsize', 0)

        self.basesize = self.BaseByteSize()  # not including flex len and flex bytes.
        self.size = self.basesize
        fs = getattr(self, 'flexsize', None)
        if fs is not None:
            self.size += fs
        if self.size & 1:
            self.padded = 1
            self.size += 1   # Final size must be even.
        else:
            self.padded = 0
        if self.size < 4:
            self.padded = 4 - self.size
            self.size = 4
        if self.size > 256:
            raise Exception("Object size too big: %d: %s", self.size, vars(self))
        self.addr = Here
        Here += self.size
        MemorableList.append(self)

    def Materialize(self):
        print 'Materialize:', self
        print 'Materialize:', vars(self)
        for k,v in vars(self).items():
            if k.startswith('b_'):
                k2 = 'B_' + k[2:]
                v2 = getattr(self, k2)
                print self, k, v, k2, v2
                Memory[self.addr + v2] = v
            if k.startswith('p_'):
                k2 = 'P_' + k[2:]
                v2 = getattr(self, k2)
                print self, k, v, k2, v2
                if isinstance(v, Memorable):
                  Memory[self.addr + v2] = Hi(v.addr)
                  Memory[self.addr + v2 + 1] = Lo(v.addr)
                elif type(v) is int:
                  Memory[self.addr + v2] = Hi(v)
                  Memory[self.addr + v2 + 1] = Lo(v)
                else:
                  raise Exception('Weird kind: %s' % type(v))
        fb = getattr(self, 'flexbytes', None)
        print ':fb:', self.basesize, fb, self
        if fb is not None:
            i = 0
            for b in fb:
                Memory[self.addr + self.basesize + i] = b
                i += 1
        for i in range(self.padded):
            Memory[self.addr + self.size - 1 - i] = '^'

    def BaseByteSize(self):
        self.Bslots = [k for k in dir(self) if k.startswith('B_')]
        self.Pslots = [k for k in dir(self) if k.startswith('P_')]
        z = len(self.Bslots) + 2*len(self.Pslots)
        assert z <= MAX_OBJ_SIZE
        return z

    def __str__(self):
        if hasattr(self, 'addr'):
          return '<%s:%s:%04x>' % (self.__class__.__name__, self.nick, self.addr)
        else:
          return '<%s:%s:????>' % (self.__class__.__name__, self.nick)
    def __repr__(self):
        return self.__str__()

class Ur(Memorable):  # Proto Object (for proxies).
    B_gcsize = 0  # Garbage Collection size, for allocator and collector.
    B_cls = 1  # Class Number.

class Obj(Ur):  # Smalltalk root Object.
    pass

class Num(Obj):
    pass

class Int(Num):  # Small 15-bit signed integer.  Encoded in an oop with low bit set.
    pass

class Addr(Num):  # 16-bit unsigned integer.
    B_hi = 2
    B_lo = 3

class NilT(Obj):  # class of nil
    pass

class Bool(Obj):
    pass

class TrueT(Bool):  # class of true
    pass

class FalseT(Bool):  # class of false
    pass

class Arr(Obj):  # LowLevel: Flexible-length abstract object.
    pass

class ArrByt(Arr):  # LowLevel: Flexible-length bytes storage.
    FLEX_BYTES = 2

class ArrPtr(Arr):  # LowLevel: Flexible-length oops storage.
    FLEX_PTRS = 2

class Tuple(ArrPtr):  # Tuple: uses fixed FlexP.
    pass

class Slice(Obj):  # LowLevel: Slice of a Flx.
    B_begin = 2
    B_len = 3
    B_intern = 4   # used in Sym for intern number.
    P_guts = 5

class Vec(Slice):  # Vector of Pointers.
    pass

class Buf(Slice):  # Buffer of Bytes.
    pass

class Str(Buf):
    pass

class Sym(Str):
    pass

class Err(Buf):
    pass

class Named(Obj):  # Object with interned name.
    B_name = 2  # Symbol index.

# The only linked lists we need is methods in a class, so no Link List class.

K_FLEX_BYTES = 1
K_FLEX_PTRS = 2
class Cls(Obj):
    B_flags = 2  # e.g. K_FLEX_BYTES, K_FLEX_PTRS
    B_bSize = 3  # Base size of instance in bytes.
    B_numB = 4   # Number of byte slots.
    B_numP = 5   # Number of Oop slots.
    B_this = 6   # This class index.
    B_partner = 7   # class to meta; meta to class.
    P_sup = 8       # Superclass, by Pointer, for faster method dispatch.
    P_meths = 10   # Head of linked list of meths.
    FLEX_BYTES = 12 # For the name of the class, so it does not have to be interned.

class Metacls(Cls):
    pass

class Meth(Named):
    B_owner = 3  # Owning Class.
    B_numL = 4   # num Locals.
    P_next = 5   # Linked list of methods on this class.
    FLEX_BYTES = 7
    # Since bytecodes are built-in flex bytes, if you recompile, you may have to
    # replace (and re-linked-list) the entire Meth object.
    # Limit of about 250 bytecodes.

class Demo(Obj):
    B_one = 2     # two byte fields: one, two.
    B_two = 3
    P_three = 4   # two oop fields: three, four.
    P_four = 6

####  Stack layout
## [
##  args
## 10 ]
## 8 Receiver
## 6 Selector Sym|Method, debug.
## 4 DE, argcount, debug.
## 2 ReturnPC
## 0 ReturnFP  <--- fp
## -2 [
##  locals
## ]         <--- sp?

# Offests from Frame pointer.
K_ARG4 = 16
K_ARG3 = 14
K_ARG2 = 12
K_ARG1 = 10
K_RCVR = 8    # conceptually, RCVR is like ARG0.
K_MSG = 6     # Needed for a debugger interpreting a stack.
K_DE = 4      # Could omit this.  It makes viewing stacks easier.
K_RET_PC = 2
K_RET_FP = 0
K_LCL0 = -2
K_LCL1 = -4
K_LCL2 = -6
K_LCL3 = -8

Method['URCLS']['new'] = '''C
    word rcvr = W(fp + K_RCVR);
    fprintf(stderr, "URCLS::new -- rcvr=%04x\\n", rcvr);
    word z = MakeInstance(rcvr, 0);
    PUSH(z);
'''
Method['URCLS']['new:'] = '''C
    word rcvr = W(fp + K_RCVR);
    fprintf(stderr, "URCLS::new -- rcvr=%04x\\n", rcvr);
    word n = W(fp + K_ARG1);
    word z = MakeInstance(rcvr, OOP2NUM(n));
    PUSH(z);
'''
Method['UR']['same'] = 'B self a same'
Method['UR']['=='] = 'B self a same'
Method['UR']['must'] = 'B self must self'  # Most objects are true.
Method['UR']['not'] = 'B self not'  # Most objects are true.

Method['UR']['bytlen'] = 'B self bytlen'
Method['UR']['ptrlen'] = 'B self ptrlen'
Method['UR']['bytat:'] = 'B a self bytat'
Method['UR']['ptrat:'] = 'B a self ptrat'
Method['UR']['bytat:put:'] = 'B b a self bytatput nil'
Method['UR']['ptrat:put:'] = 'B b a self ptratput nil'
Method['ARRBYT']['len'] = 'B self bytlen'
Method['ARRBYT']['at:'] = 'B a self bytat'
Method['ARRBYT']['at:put:'] = 'B b self a bytatput nil'
Method['ARRPTR']['len'] = 'B self ptrlen'
Method['ARRPTR']['at:'] = 'B a self ptrat'
Method['ARRPTR']['at:put:'] = 'B b self a ptratput nil'

Method['DEMO']['run'] = '''B
    lit_b 51 self #double: lit_w %d %d call dup show
    lit_b 51 self #twice: lit_w %d %d call dup show
    add dup show
''' % (0xDE, 0x01, 0xDE, 0x01)

Method['DEMO']['run2setup'] = '''T
    acct = Demo new init.
            acct balance show.
    acct deposit: 10.
            acct balance show.
    acct deposit: 100.
            acct balance show.
    acct withdraw: 20.
            acct balance show.
'''
Method['DEMO']['run2'] = '''T
    self run2setup.
    IF( 5 )THEN( 5 show ).
    IF( true )THEN( 42 show )ELSE( 666 show ).
    n = 3.
    WHILE( n )DO( n show. n = n - 1. ).
    FOR( i : 5 )DO( i show ).

    p = ArrByt new: 5.
    FOR( i : 5 )DO( p bytAt: i put: 10 + i ).
    FOR( i : 5 )DO( (p bytAt: i) show ).
    FOR( i : 5 )DO( ((p bytAt: i) == i) must ).
    p bytLen show.
'''

Method['DEMO']['double:'] = 'B arg1 arg1 add '  # Using Bytecodes.
Method['DEMO']['twice:'] = 'T a + a '           # Using TerseTalk.

Method['DEMO']['init'] = 'T one = 0. self'
Method['DEMO']['deposit:'] = 'T one = one + a. nil'
Method['DEMO']['withdraw:'] = 'T one = one - a. nil'
Method['DEMO']['balance'] = 'T one'

Method['INT']['+'] = 'B self arg1 add '
Method['INT']['-'] = 'B self arg1 subtract '
Method['INT']['show'] = 'B self show self'

Op['stop'] = ' goto STOP; '

Op['self'] = '  PUSH(W(fp+K_RCVR));'
Op['arg1'] = '  PUSH(W(fp+K_ARG1));'
Op['arg2'] = '  PUSH(W(fp+K_ARG2));'
Op['arg3'] = '  PUSH(W(fp+K_ARG3));'
Op['a'] = '  PUSH(W(fp+K_ARG1));'
Op['b'] = '  PUSH(W(fp+K_ARG2));'
Op['c'] = '  PUSH(W(fp+K_ARG3));'
Op['d'] = '  PUSH(W(fp+K_ARG4));'
Op['cls_b'] = ' byte n = BYTE(pc); pc += 1; PUSH(ClassVec[n]); '

Op['clsof'] = ' word x = PEEK(0); POKE(0, CLASSOF(x)); '
Op['same'] = ' word x = POP(); word y = PEEK(0); POKE(0, (x==y));'

Op['bytlen'] = ''' // p -> len
    word p = PEEK(0);
    POKE(0, NUM2OOP(BytLen(p)));
'''
Op['ptrlen'] = ''' // p -> len
    word p = PEEK(0);
    POKE(0, NUM2OOP(PtrLen(p)));
'''
Op['bytat'] = ''' // p i -> b
    word i = OOP2NUM(POP());
    word p = PEEK(0);
    POKE(0, NUM2OOP(BytAt(p, i)));
'''
Op['ptrat'] = '''
    word i = OOP2NUM(POP());
    word p = PEEK(0);
    POKE(0, PtrAt(p, i));
'''
Op['bytatput'] = '''
    word v = OOP2NUM(PEEK(0));
    word i = OOP2NUM(PEEK(2));
    word p = PEEK(4);
    BytAtPut(p, i, v);
    sp += 6;
'''
Op['ptratput'] = '''
    word v = PEEK(0);
    word i = OOP2NUM(PEEK(2));
    word p = PEEK(4);
    PtrAtPut(p, i, v);
    sp += 6;
'''

Op['forward_jump_b'] = '''
    byte n = BYTE(pc); pc += 1;
    pc += n;
'''

Op['reverse_jump_b'] = '''
    byte n = BYTE(pc); pc += 1;
    pc -= n;
'''

Op['forward_bfalse_b'] = '''
    byte n = BYTE(pc); pc += 1;
    word x = POP();
    if (!Truth(x)) {
        pc += n;
    }
'''

Op['reverse_bfalse_b'] = '''
    byte n = BYTE(pc); pc += 1;
    word x = POP();
    if (!Truth(x)) {
        pc -= n;
    }
'''

# Get/Put pointer slots.
Op['getp_b'] = '''
    byte n = BYTE(pc); pc += 1;
    word obj = PEEK(0);
    word x = W(obj + n);
    POKE(0, x);
'''
Op['putp_b'] = '''
    byte n = BYTE(pc); pc += 1;
    word obj = POP();
    word x = POP();
    PUT_WORD(obj+n, x);
'''

# Get/Put byte slots: values are unsigned integers in 0..255.
Op['getb_b'] = '''
    byte n = BYTE(pc); pc += 1;
    word obj = PEEK(0);
    byte x = B(obj + n);
    POKE(0, NUM2OOP(x));
'''
Op['putb_b'] = '''
    byte n = BYTE(pc); pc += 1;
    word obj = POP();
    word x = POP();
    CHECK3(x&1, 1, x);
    CHECK3(x&0xFE00, 0, x);
    PUT_BYTE(obj+n, OOP2BYTE(x));
'''

# Store/Recall local variables.
Op['sto0'] = '  word w = POP(); PUT_WORD(fp-2, w);'
Op['sto1'] = '  word w = POP(); PUT_WORD(fp-4, w);'
Op['sto2'] = '  word w = POP(); PUT_WORD(fp-6, w);'
Op['sto3'] = '  word w = POP(); PUT_WORD(fp-8, w);'
Op['sto_b'] = ' byte n = BYTE(pc); pc += 1; word w = POP(); PUT_WORD(fp-2*(n+1), w);'
Op['rcl0'] = '  word w = W(fp-2); PUSH(w);'
Op['rcl1'] = '  word w = W(fp-4); PUSH(w);'
Op['rcl2'] = '  word w = W(fp-6); PUSH(w);'
Op['rcl3'] = '  word w = W(fp-8); PUSH(w);'
Op['rcl_b'] = ' byte n = BYTE(pc); pc += 1; word w = W(fp-2*(n+1)); PUSH(w);'
Op['incr_local_b'] = 'byte n = BYTE(pc); pc += 1; word p = fp-2*(n+1); word w = W(p); PUT_WORD(p, w+2);'

Op['true'] = '  PUSH(trueAddr);'
Op['false'] = '  PUSH(falseAddr);'
Op['nil'] = '  PUSH(nilAddr);'
Op['show'] = '  word w = POP(); printf(" ==$%04x=%u.==\\n", w, w); fflush(stdout); '

Op['lit2pcr'] = '''
  PUSH(WORD(pc) - pc);
  pc += 2;
'''

Op['lit1pcr'] = '''
  byte n = BYTE(pc);
  word w = ((word)n) | ((n & 128) ? 0xFF00U : 0x0000U);  // SEX.
  PUSH(w - pc);
  pc += 1;
'''

Op['lit_w'] = '''
  PUSH(WORD(pc));
  pc += 2;
'''

Op['lit_b'] = '''
  byte n = BYTE(pc);
  word w = (0x80&n) ? (0xFF80 | (word)n) : (word)n;  // SEX.
  PUSH(w);
  pc += 1;
'''

Op['sym_b'] = '''
  byte n = BYTE(pc);
  PUSH(SymVec[n]);
  pc += 1;
'''

Op['get'] = '''
  word off = PEEK(0);
  POKE(0, WORD(fp+off));
'''

Op['drop'] = '''
  sp += 2;
'''
Op['dup'] = '''
  word top = PEEK(0);
  PUSH(top);
'''

Op['lognum'] = '''
  word id = POP();
  word value = POP();
  fprintf(stderr, "%04x:=:%04x ", id, value);
'''
Op['must'] = '''
  word x = POP();
  CHECK3(Truth(x), 1, pc);
'''
Op['not'] = '''
  word x = PEEK(0);
  POKE(0, Truth(x) ? trueAddr : falseAddr);
'''
Op['add'] = '''
  word a = POP();
  CHECK3(a&1, 1, a);
  word b = PEEK(0);
  CHECK3(b&1, 1, b);
  POKE(0, (0xFFFE & a)+b);
'''

Op['subtract'] = '''
  word a = POP();
  CHECK3(a&1, 1, a);
  word b = PEEK(0);
  CHECK3(b&1, 1, b);
  word nega = (~a) + 2;
  fprintf(stderr, "subtract a=%04x b=%04x nega=%04x z=%04x\\n", a, b, nega, b+nega);
  POKE(0, b+nega);
'''

Op['call0_b'] = '''
  byte msg = BYTE(pc);
  pc += 1;
  word rcvr = PEEK(0); 
  
  PUSH(msg);
  PUSH(0xDE00);
  PUSH(pc);
  PUSH(fp);
  fprintf(stderr, "Old FP = $%04x\\n", fp);
  fp = sp;
  fprintf(stderr, "New FP = $%04x\\n", fp);
  Hex20("STACK fp,pc,de,msg,rcvr...", sp, sp);

  word meth = FindMethBySymbolNumber(rcvr, msg);
  byte i;
  byte numL = B(meth + METH_B_numL);
  for (i=0; i<numL; i++) {
    PUSH(nilAddr);
  }
  pc = meth + METH_FLEXSIZE;
'''

Op['call1_b'] = '''
  byte msg = BYTE(pc);
  pc += 1;
  word rcvr = PEEK(0); 
  
  PUSH(msg);
  PUSH(0xDE01);
  PUSH(pc);
  PUSH(fp);
  fprintf(stderr, "Old FP = $%04x\\n", fp);
  fp = sp;
  fprintf(stderr, "New FP = $%04x\\n", fp);
  Hex20("STACK fp,pc,de,msg,rcvr...", sp, sp);

  word meth = FindMethBySymbolNumber(rcvr, msg);
  byte i;
  for (i=0; i<B(meth + METH_B_numL); i++) {
    PUSH(nilAddr);
  }
  pc = meth + METH_FLEXSIZE;
'''

Op['call2_b'] = '''
  byte msg = BYTE(pc);
  pc += 1;
  word rcvr = PEEK(0); 
  
  PUSH(msg);
  PUSH(0xDE02);  // This is all that changes..... TODO
  PUSH(pc);
  PUSH(fp);
  fprintf(stderr, "Old FP = $%04x\\n", fp);
  fp = sp;
  fprintf(stderr, "New FP = $%04x\\n", fp);
  Hex20("STACK fp,pc,de,msg,rcvr...", sp, sp);

  word meth = FindMethBySymbolNumber(rcvr, msg);
  byte i;
  for (i=0; i<B(meth + METH_B_numL); i++) {
    PUSH(nilAddr);
  }
  pc = meth + METH_FLEXSIZE;
'''

Op['call'] = '''
  word rcvr = PEEK(4); 
  //Hex20("call--rcvr", rcvr, rcvr);
  Inspect(rcvr, "call--rcvr");
  word msg = PEEK(2); 
  //Hex20("call--msg", msg, -1);
  PrintSymNum(msg, "call--msg");
  Inspect(SymVec[msg], "call--msg");
  word de = PEEK(0); 
  Hex20("call--de", de, -1);
  CHECK3(de & 0xFFF0, 0xDE00, de);
  PUSH(pc);
  PUSH(fp);
  fprintf(stderr, "Old FP = $%04x\\n", fp);
  fp = sp;
  fprintf(stderr, "New FP = $%04x\\n", fp);
  Hex20("STACK fp,pc,de,msg,rcvr...", sp, sp);

  word meth = FindMethBySymbolNumber(rcvr, msg);
  Inspect(meth, "call--meth");
  byte i;
  byte num_locals = BF(meth, METH_B_numL);
  fprintf(stderr, "Num Locals = %d\\n", num_locals);
  for (i=0; i<num_locals; i++) {
    PUSH(nilAddr);
  }
  if (BytLen(meth)) {
    pc = FlexAddrAt(meth, 0);
  }
'''

Op['return'] = '''
  word result = PEEK(0);
  sp = fp;
  fp = POP();
  fprintf(stderr, "Popped FP = $%04x\\n", fp);
  if (!fp) {
    fprintf(stderr, "Finishing with Zero FP.\\n");
    goto STOP;
  }
  pc = POP();
  if (!pc) {
    fprintf(stderr, "Finishing with Zero PC.\\n");
    goto STOP;
  }
  fprintf(stderr, "Popped PC = $%04x\\n", pc);

  word nargs = POP();
  fprintf(stderr, "Peeked nargs = $%04x\\n", nargs);
  nargs &= 255;
  sp += 2 * (nargs + 2 /* msg, rcvr*/ );
  PUSH(result);
'''

def AllClassesPreorder(start=Ur):
    z = [start]
    for sub in sorted(start.__subclasses__(), key=lambda c: c.__name__):
        z += AllClassesPreorder(sub)
    return z

# Gather all Konstants.
Konsts = {}
for k, v in globals().items():
    if k.startswith('K_'):
        Konsts[k] = v

# First create NIL, FALSE, TRUE instances, in that order.
NIL, FALSE, TRUE = NilT(), FalseT(), TrueT()
NIL.Reify(), FALSE.Reify(), TRUE.Reify()

def FixSlotsOnClass(c, inst):
    print 'FNORD', inst.nick, dir(c)
    # Check the bslots, pslots, & flexes; compute b_flags, b_bSize, b_numB, b_numP
    bslots = [(k, getattr(c, k)) for k in dir(c) if k.startswith('B_')]
    pslots = [(k, getattr(c, k)) for k in dir(c) if k.startswith('P_')]
    flexes = [(k, getattr(c, k)) for k in dir(c) if k.startswith('FLEX_')]
    bslots = sorted(bslots, key=lambda pair: pair[1])
    pslots = sorted(pslots, key=lambda pair: pair[1])
    print 'cBPF', c, bslots, pslots, flexes
    for i, (k, v) in zip(range(len(bslots)), bslots):
        if i != v: raise Exception("Bad B_ numbers in class %s: %s" % (c, bslots))
    for i, (k, v) in zip(range(len(pslots)), pslots):
        if len(bslots)+2*i != v: raise Exception("Bad P_ numbers in class %s: %s" % (c, pslots))
    inst.b_numB = len(bslots)
    inst.b_numP = len(pslots)
    inst.b_bSize = inst.b_numB + 2*inst.b_numP
    if flexes:
        assert len(flexes) == 1  # ThereCanOnlyBeOne
        if flexes[0][0]=='FLEX_BYTES':
            inst.b_flags = K_FLEX_BYTES
        elif flexes[0][0]=='FLEX_PTRS':
            inst.b_flags = K_FLEX_PTRS
        else:
            raise Exception('Bad FLEX records in class %s: %s' % (c, flexes))
    else:
        inst.b_flags = 0
    inst.bslots = bslots
    inst.pslots = pslots
    inst.flexes = flexes

# Create class objects.
ClassDict = {}
for c in AllClassesPreorder():
    inst = Cls()
    inst.pycls = c
    inst.nick = c.__name__
    inst.name = c.__name__.upper()
    inst.flexstring = inst.name
    inst.flexsize = len(inst.name)
    inst.flexbytes = [ord(s) for s in inst.name]
    inst.b_this = len(ClassDict) + 1  # Skip the 0 class, meaning unused memory.
    ClassVec[inst.b_this] = inst
    inst.sup = None if c is Ur else c.__bases__[0]
    inst.p_sup = NIL if c is Ur else ClassDict[c.__bases__[0].__name__.upper()]
    inst.p_meths = NIL
    inst.Reify()
    ClassDict[inst.name] = inst
    FixSlotsOnClass(c, inst)

def WriteInspectors():
  for cname, c in sorted(ClassDict.items()):
    print 'struct FieldInfo FI_%s[] = {' % (
            c.name)
    for bs in c.bslots:
        fname, foff = bs
        print '  { "%s", 1, %d }, ' % (fname, foff)
    for ps in c.pslots:
        fname, foff = ps
        print '  { "%s", 2, %d }, ' % (fname, foff)
    print '  { NULL, 0, 0 }'
    print '};'

    print '''
struct ClassInfo CI_%s = {
        "%s",
        %d,
        FI_%s};
    ''' % (c.name, c.name, c.b_this, c.name)

  print 'void InitInfo() {'
  for cname, c in sorted(ClassDict.items()):
      print '  ClassInfos[%d] = &CI_%s;' % (
        c.b_this, c.name)
  print '}'

# Create metaclass objects.
METACLS = ClassDict['METACLS']
for c in AllClassesPreorder():
    meta = Metacls()
    meta.nick = c.__name__ + 'ClS'
    meta.name = c.__name__.upper() + 'CLS'
    if True:
        meta.flexstring = meta.name
        meta.flexsize = len(meta.name)
        meta.flexbytes = [ord(s) for s in meta.name]
    else:
        meta.flexstring = ''
        meta.flexsize = 0
        meta.flexbytes = []
    meta.b_this = len(ClassDict) + 1  # Skip the 0 class, meaning unused.
    ClassVec[meta.b_this] = meta
    meta.p_sup = ClassDict['CLS'] if c is Ur else ClassDict[c.__bases__[0].__name__.upper() + 'CLS']
    meta.sup = meta.p_sup
    meta.p_meths = NIL
    meta.Reify()
    ClassDict[meta.name] = meta
    FixSlotsOnClass(METACLS, meta)

# Link metaclass class objects.
for c in AllClassesPreorder():
    meta = ClassDict[c.__name__.upper() + 'CLS']
    meta.b_cls = METACLS.b_this
    inst = ClassDict[c.__name__.upper()]
    inst.b_cls = meta.b_this
    inst.b_partner = meta.b_this
    meta.b_partner = inst.b_this

#### Compile methods and intern symbols.
Op = dict([(k.upper(), v) for k,v in Op.items()])  # Normalize keys upper.
Method = dict([(k.upper(), v) for k,v in Method.items()])  # Normalize keys upper.
for k,v in Method.items():  # Also normalize inner keys (method names).
    for k2 in v:
        Intern(k2.upper())
    Method[k] = dict([(k2.upper(), v2) for k2,v2 in v.items()])

print '=== Op:', repr(Op)
print '=== Method:', repr(Method)

for cname, m in sorted(Method.items()):
    for mname, v in sorted(m.items()):
        Intern(mname.upper())
OpList = ['STOP'] + sorted([k.upper() for k in Op if k != 'STOP'])
for i, op in zip(range(len(OpList)), OpList):
    OpNums[op] = i
print '=== OpNums:', repr(OpNums)

def CompileMethod(cname, mname, v):
    numL = 2
    if v[0] == 'T':
        codes, numL = CompileToCodes(v[1:], ClassDict[cname])
        # Change to format 'B' for text bytecode string.
        v = 'B ' + ' '.join([str(c) for c in codes])

    if v[0] == 'C':
        # Create an opcode for the C code.
        opname = ('%s_%s_c' % (cname, mname)).upper().replace(':', '_')
        Op[opname] = v[1:]
        opnum = len(OpList)
        OpList.append(opname)
        OpNums[opname] = opnum
        # Now pretend it was a B definition, to call the new opcode.
        v = 'B %s' % opname

    if v[0] != 'B':
        raise Exception('Only Bytecode (B) supported: %s' % repr(v))
    v = v[1:]
    explain, codes = [], []
    ww = v.split()
    print 'Compiling (%s %s): %s' % (cname, mname, ww)
    Marks = {}
    Fixes = []
    for w in ww:
        if INT_PATTERN(w):
            explain.append(EvalInt(w))
            codes.append(EvalInt(w))
        elif SYM_PATTERN(w):
            num = Intern(SYM_PATTERN(w).group(1).upper())
            explain.append('lit_b')
            explain.append(num)
            codes.append(OpNums['LIT_B'])
            codes.append(num)
        elif CLASS_PATTERN(w):
            cn = CLASS_PATTERN(w).group(1).upper()
            c = ClassDict.get(cn)
            explain.append('CLASS_B')
            explain.append(c.b_this)
            codes.append(OpNums['CLASS_B'])
            codes.append(c.b_this)
        elif MARK_PATTERN(w):
            verb, target = MARK_PATTERN(w).groups()
            target = int(target)
            if verb == 'mark':
                Marks[target] = len(codes)
            elif verb == 'jump':
                Fixes.append((target, len(codes)))
                codes.append('jump_b')
                codes.append(0)
            elif verb == 'bfalse':
                Fixes.append((target, len(codes)))
                codes.append('bfalse_b')
                codes.append(0)
            else:
                raise 'bad'
        else:
            num = OpNums.get(w.upper())
            if num is None:
                raise Exception('No such opcode: [%s %s]: %s: %s' % (cname, mname, w, repr(v)))
            explain.append(w)
            codes.append(OpNums[w.upper()])
    explain.append('RETURN');
    codes.append(OpNums['RETURN']);

    for (mark, loc) in Fixes:
        target = Marks[mark]
        if target < loc:
            codes[loc] = OpNums[('reverse_' + codes[loc]).upper()]
            codes[loc+1] = loc + 2 - target
        else:
            codes[loc] = OpNums[('forward_' + codes[loc]).upper()]
            codes[loc+1] = target - loc - 2

    print 'CompileMethod: %s %s: %s' % (cname, mname, explain)
    print 'CompileMethod: %s %s: %s' % (cname, mname, codes)
    return explain, codes, numL

CompiledMethods = {}
for cname, m in sorted(Method.items()):
    cname = cname.upper();
    print 'CNAME: %s METHODS: %s' % (cname, m)
    for mname, v in sorted(m.items()):
        mname = mname.upper();
        explain, codes, numL = CompileMethod(cname, mname, v)
        CompiledMethods[(cname, mname)] = (codes, numL)

for (cname,mname),(codes,numL) in sorted(CompiledMethods.items()):
    meth = Meth()
    cls = ClassDict[cname]
    meth.b_cls = ClassDict['METH'].b_this
    meth.b_name = Intern(mname.upper())
    meth.b_owner = cls.b_this
    meth.b_numL = numL
    meth.p_next = cls.p_meths  # prepend to linked list.
    cls.p_meths = meth
    meth.flexsize = len(codes)
    meth.flexbytes = codes
    meth.Reify()

# Prepare packed strings with all interned symbols.
InternLoc = {}
InternSym = {}
PackedStrings = ['']
for (k, v) in InternDict.items():
    s = PackedStrings[-1]
    if len(s) + len(k) > MAX_FLEX_BYTES:
        s = ''
        PackedStrings.append(s)
    InternLoc[k] = (len(PackedStrings), len(s)) 
    s += k
    PackedStrings[-1] = s  # Put the new string back.
# Reify the interned symbols.
for (k, v) in InternDict.items():
    sym = Sym()
    sym.b_intern = v
    sym.Reify()
    SymVec[v] = sym
    InternSym[k] = sym
# Reify the packed strings.
PackedList = []
for ps in PackedStrings:
    po = ArrByt()
    po.flexstring = ps
    po.flexsize = len(ps)
    po.flexbytes = [ord(s) for s in ps]
    assert len(po.flexbytes) == po.flexsize
    po.Reify()
    print 'PackedString:', po.nick, po.addr, po.basesize, po.flexsize, po.flexbytes
    PackedList.append(po)
# Fill in symbol fields.
for (k, v) in InternSym.items():
    v.str = k
    packNum, offset = InternLoc[k]
    v.b_begin = offset
    v.b_len = len(k)
    v.p_guts = PackedList[packNum-1].addr

for m in MemorableList:
    m.b_gcsize = ((m.size-2)>>1, m.nick, m.addr, m.basesize, m.__class__.__name__)
    assert m.b_gcsize[0] > 0, vars(m)
    assert m.b_gcsize[0] < 128, vars(m)
    m.CLS = ClassDict[m.__class__.__name__.upper()]
    m.b_cls = m.CLS.b_this
    m.Materialize()

pass;pass;pass

def GenerateH():
    print '''/* This is Generated Code */
#ifndef TERSETALK9_GENERATED_H_
#define TERSETALK9_GENERATED_H_
#include "vm.h"
'''
    for k,v in globals().items():
        if k.startswith('K_'):
            print '#define %s %s' % (k, v)

    for op in OpList:
        print '#define OP_%-17s %d' % (op, OpNums[op])
    print

    for c in ClassVec[1:]:
        if not c: continue

        i = 0
        for e, off in c.bslots:
            assert i == off
            print '  #define %s_%s %d' % (c.name, e, off)
            i += 1

        for e, off in c.pslots:
            assert i == off
            print '  #define %s_%s %d' % (c.name, e, off)
            i += 2

        if c.flexes:
            print '  #define %s_FLEXSIZE %d' % (c.name, c.flexes[0][1])
            i += 1

        print
    print '#endif'


def GenerateC():
    print '''/* This is Generated Code */
#include "vm.h"
#include "_generated.h"
'''

    print '''
#ifdef DEBUG
char* OpNames[] = {
'''
    for e in OpList:
        print '    "%s",' % e
    print '''
    NULL,
};
#endif
'''

    print '''
void Boot() {
'''
    print '  nilAddr = 0x%04x;' % NIL.addr
    print '  falseAddr = 0x%04x;' % FALSE.addr
    print '  trueAddr = 0x%04x;' % TRUE.addr
    print '  intAddr = 0x%04x;' % ClassDict['INT'].addr
    print '  clsAddr = 0x%04x;' % ClassDict['CLS'].addr
    print '''
}
'''

    print '''
void Loop() {
  while (1) {
#ifdef DEBUG
    Hex20("pc", pc, pc);
    Hex20("fp", fp, fp);
    Hex20("sp", sp, sp);
#endif
    byte opcode = BYTE(pc);
    ++pc;
#ifdef DEBUG
    fprintf(stderr, "Step: opcode: $%02x=%d.=%s\\n", opcode, opcode, OpNames[opcode]);
#endif
    switch (opcode) {
'''
    for op in OpList:
        print '\tcase OP_%s: {' % op
        print '\t  fprintf(stderr, "OP: %s\\n");' % op
        for k,v in Op.items():
            done = False
            if k.upper() == op:
                if done:
                    raise Exception('already done Op[%s]' % op)
                for s in v.split('\n'):
                    print '\t\t%s' % s
                done = True
        print '\t}'
        print '\tbreak;'
        print

    print '''
    }
  }
STOP:
  return;
}
'''

def GenerateImage():
    def w(x): sys.stdout.write(x)
    def w2(x): w('%c%c' % (Hi(x), Lo(x)))
    def w1(x): w('%c' % x)

    # Terse talk version number 1
    w('T/'); w2(2); w2(1)

    # Class vector
    n = len(ClassDict)
    w('C/'); w2(n*2)
    for c in ClassVec[:n]:
        w2(0 if c==0 else c.addr)  # Initial 0 for unused mem.

    # Intern Symbol vector
    n = len(InternDict)
    w('S/'); w2(n*2)
    for y in SymVec[:n]:
        w2(y.addr)

    # Memory bytes
    n = Here
    w('M/'); w2(n)
    for x in Memory[:n]:
        if x == '^': w1(0)    # padding
        #elif x == '#': raise 'bad' # w1(0)     # unused part of page
        #elif x == '@': raise 'bad' # w1(0)  # last in page
        elif type(x) is tuple: w1(x[0])
        elif type(x) is int: w1(x)
        else: raise Exception('weird memory: %s' % x)

    # End with a zero-length segment with '!/' name.
    w('!/'); w2(0);
    pass

print dir(TRUE)
print vars(TRUE)
print
for m in MemorableList:
    print 'M:', m, vars(m)
print
for name, cls in ClassDict.items():
    print name, cls, (cls.__class__.__bases__)

print
print 'SymVec:', SymVec
print
print 'InternSym:', len(InternSym), InternSym
print
print 'InternDict:', len(InternDict), InternDict
print
print 'ClassDict:', len(ClassDict), ClassDict
print
print 'ClassVec:', ClassVec
print
print 'OpList:', OpList
print
print 'Memory:', Here, Memory[:Here]
print
for resource, used, maximum in [
        ('Memory', Here, 65536),
        ('Classes', len(ClassDict), 255),
        ('Symbols', len(InternDict), 256),
        ('Opcodes', len(OpNums), 256),
    ]:
        print '%10s  %5.2f%%  %6d/%6d full' % (resource, 100.0 * used / maximum, used, maximum)


sys.stdout = open('_generated.h', 'w')
GenerateH()
sys.stdout.close()

sys.stdout = open('_generated.c', 'w')
GenerateC()
WriteInspectors()
sys.stdout.close()

sys.stdout = open('_generated.image', 'wb')
GenerateImage()
sys.stdout.close()

pass
