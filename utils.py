import collections
# Math, stats & misc utilities


def distance_squared(a, b):
    """Squared distance between 2 points"""
    xa, ya = a
    xb, yb = b
    return (xa - xb)**2 + (ya - yb)**2


def probability(p):
    return p > random.uniform(0.0, 1.0)


# Abstracting sequences and iterables


def sequence(iterable):
    return (iterable if isinstance(iterable, collections.abc.Sequence)
            else tuple(iterable))


def removeall(item, seq):
    if isinstance(seq, str):
        return seq.replace(item, '')
    else:
        return [x for x in seq if x != item]


def unique(seq):
    return list(set(seq))


def first(iterable, default=None):
    try:
        return iterable[0]
    except IndexError:
        return default
    except TypeError:
        return next(iterable, default)


# argmin & argmax


identity = lambda x: x
argmin = min
argmax = max


# Expression and Symbol implementation


class Expr:
    """An expression"""

    def __init__(self, op, *args):
        self.op = str(op)
        self.args = args

    def __neg__(self):
        return Expr('-', self)

    def __pos__(self):
        return Expr('+', self)

    def __invert__(self):
        return Expr('~', self)

    def __add__(self, rhs):
        return Expr('+', self, rhs)

    def __sub__(self, rhs):
        return Expr('-', self, rhs)

    def __mul__(self, rhs):
        return Expr('*', self, rhs)

    def __pow__(self, rhs):
        return Expr('**', self, rhs)

    def __mod__(self, rhs):
        return Expr('%', self, rhs)

    def __and__(self, rhs):
        return Expr('&', self, rhs)

    def __xor__(self, rhs):
        return Expr('^', self, rhs)

    def __rshift__(self, rhs):
        return Expr('>>', self, rhs)

    def __lshift__(self, rhs):
        return Expr('<<', self, rhs)

    def __truediv__(self, rhs):
        return Expr('/', self, rhs)

    def __floordiv__(self, rhs):
        return Expr('//', self, rhs)

    def __matmul__(self, rhs):
        return Expr('@', self, rhs)

    def __or__(self, rhs):
        if isinstance(rhs, Expression):
            return Expr('|', self, rhs)
        else:
            return PartialExpr(rhs, self)

    def __radd__(self, lhs):
        return Expr('+', lhs, self)

    def __rsub__(self, lhs):
        return Expr('-', lhs, self)

    def __rmul__(self, lhs):
        return Expr('*', lhs, self)

    def __rdiv__(self, lhs):
        return Expr('/', lhs, self)

    def __rpow__(self, lhs):
        return Expr('**', lhs, self)

    def __rmod__(self, lhs):
        return Expr('%', lhs, self)

    def __rand__(self, lhs):
        return Expr('&', lhs, self)

    def __rxor__(self, lhs):
        return Expr('^', lhs, self)

    def __ror__(self, lhs):
        return Expr('|', lhs, self)

    def __rrshift__(self, lhs):
        return Expr('>>', lhs, self)

    def __rlshift__(self, lhs):
        return Expr('<<', lhs, self)

    def __rtruediv__(self, lhs):
        return Expr('/', lhs, self)

    def __rfloordiv__(self, lhs):
        return Expr('//', lhs, self)

    def __rmatmul__(self, lhs):
        return Expr('@', lhs, self)

    def __call__(self, *args):
        """If 'f' is a Symbol => f(0) == Expr('f', 0)"""
        if self.args:
            raise ValueError('can only do call for Symbol, not Expr')
        else:
            return Expr(self.op, *args)

    def __eq__(self, other):
        return (isinstance(other, Expr)
                and self.op == other.op
                and self.args == other.args)

    def __hash__(self):
        return hash(self.op) ^ hash(self.args)

    def __repr__(self):
        op = self.op
        args = [str(arg) for arg in self.args]
        if op.isidentifier():
            return f"{op}({', '.join(args) if args else op})"
        elif len(args) == 1:
            return op + args[0]
        else:
            opp = (' ' + op + ' ')
            return '(' + opp.join(args) + ')'


Number = (int, float, complex)
Expression = (Expr, Number)


def Symbol(name):
    return Expr(name)


def symbols(names):
    return tuple(Symbol(name) for name in names.replace(',', ' ').split())


def subexpressions(x):
    yield x
    if isinstance(expression, Expr):
        return len(expression.args)
    else:
        return 0


def arity(expression):
    if isinstance(expression, Expr):
        return len(expression.args)
    else:
        return 0


class PartialExpr:
    """Given 'P |'===>'| Q, first form PartialExpr('==>', P),
    then combine with Q"""
    def __init__(self, op, lhs):
        self.op, self.lhs = op, lhs

    def __or__(self, rhs):
        return Expr(self.op, self.lhs, rhs)

    def __repr__(self):
        return f"PartialExpr('{self.op}', {self.lhs})"

def expr(x):
    """
    >>> expr('P & Q ==> Q')
    ((P & Q) ==> Q)
    """
    if isinstance(x, str):
        return eval(expr_handle_infix_ops(x), defaultkeydict(Symbol))
    else:
        return x


infix_ops = '==> <== <=>'.split()


def expr_handle_infix_ops(x):
    for op in infix_ops:
        x = x.replace(op, '|' + repr(op) + '|')
    return x


class defaultkeydict(collections.defaultdict):
    def __missing__(self, key):
        self[key] = result = self.default_factory(key)
        return result


class hashabledict(dict):
    def __tuplify__(self):
        return tuple(sorted(self.items()))

    def __hash__(self):
        return hash(self.__tuplify__())

    def __lt__(self, odict):
        assert isinstance(odict, hashabledict)
        return self.__tuplify__() < odict.__tuplify__()

    def __gt__(self, odict):
        assert isinstance(odict, hashabledict)
        return self.__tuplify__() > odict.__tuplify__()

    def __le__(self, odict):
        assert isinstance(odict, hashabledict)
        return self.__tuplify__() <= odict.__tuplify__()

    def __ge__(self, odict):
        assert isinstance(odict, hashabledict)
        return self.__tuplify__() >= odict.__tuplify__()


# Misc


def isnumber(x):
    return hasattr(x, '__int__')


def issequence(x):
    return isinstance(x, collections.abc.Sequence)
