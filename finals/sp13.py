# 5a
def rev(l):
    return [x for x in l[::-1]]

assert(rev([1,2,3]) == [3,2,1])

# 5b
def rev(l):
    return reduce(lambda a, n: [n] + a, l, [])

assert(rev([1,2,3]) == [3,2,1])

# 6
def print_some(l):
    print_ret = -1 in l
    def decorator(f):
        def decorated(*args):
            for x in l:
                if x >= 0:
                    try:
                        print "Arg %d: %d" % (x, args[x])
                    except IndexError:
                        pass
            result = f(*args)
            if print_ret:
                print "Return: %d" % result
            return result
        return decorated
    return decorator

@print_some([-1,1,0])
def plus(x, y):
    print "-- plus called --"
    return x + y

print(plus(1, 2))

@print_some([-2, 100])
def plus(x, y):
    print "-- plus called --"
    return x + y

print(plus(1, 2))

@print_some([-1, 0])
def fac(n):
    print "-- fac called --"
    if n is 0: return 1
    else: return n * fac(n - 1)

print(fac(2))

# 7a
class Tree:
    def __init__(self, name, children):
        self.name = name
        self.children = children

    # Returns True if the Tree represents a prolog variable (e.g. X),
    # and False otherwise
    def is_var(self):
        try:
            int(self.name)
            return False
        except ValueError:
            return self.children == []

    # Returns the string representation of the Tree as a Prolog term.
    def __repr__(self):
        children = ', '.join(map(repr, self.children))
        return '%s%s' % (self.name, '(%s)' % children if children else '')

# Constructs a Tree representing a Prolog variable with name n
def var(n):
    return Tree(n, [])

# Constructs a Tree representing a non-variable term with name n
# and children c
def node(n, c):
    return Tree(n, c)

def apply_to_tree(s, t):
    if not t.is_var():
        return node(t.name, [apply_to_tree(s, c) for c in t.children])
    elif t.name in s:
        return apply_to_tree(s, s[t.name])
    else:
        return t

s1 = {}
s1['X'] = node('foo', [node(5, [])])
s2 = s1.copy()
s2['Y'] = node('baz', [node(10, []), var('X')])
t1 = node('bat', [var('X')])
t2 = node('bat', [var('X'), var('Y')])

assert(repr(apply_to_tree(s1, t1)) == 'bat(foo(5))')
assert(repr(apply_to_tree(s2, t2)) == 'bat(foo(5), baz(10, foo(5)))')
assert(repr(apply_to_tree(s1, t2)) == 'bat(foo(5), Y)')

# 7b
def unify(a, b, s={}):
    a = apply_to_tree(s, a)
    b = apply_to_tree(s, b)
    result = s.copy()
    if a.is_var() and b.is_var():
        result[a.name] = b
    elif a.is_var() and not b.is_var():
        if a.name in result:
            unify(result[a.name], b, result)
        else:
            result[a.name] = b
    elif not a.is_var() and b.is_var():
        return unify(b, a, s)
    elif not a.is_var() and not b.is_var():
        if a.name != b.name:
            return False
        if len(a.children) != len(b.children):
            return False
        for ca, cb in (cs for cs in zip(a.children, b.children)
                       if result is not False):
            result = unify(ca, cb, result)
    return result

t1 = node("foo", [var("X"),node(5,[])])
t2 = node("foo", [node(10,[]),var("Y")])
t3 = node("foo", [node(10,[]),var("X")])
t4 = node("bar", [var("X"),var("Y"),var("X")])
t5 = node("bar", [node("foo", [var("Y")]),node("3",[]),node("foo", [node("3",[])])])
t6 = node("bar", [node("foo", [var("Y")]),node("3",[]),node("foo", [node("4",[])])])

assert(repr(unify(t1,t2)) == repr({'Y': 5, 'X': 10}))
assert(repr(unify(t1,t3)) == repr(False))
assert(repr(unify(t4,t5)) == repr({'Y': 3, 'X': node('foo', [var('Y')])}))
assert(repr(unify(t4,t6)) == repr(False))
