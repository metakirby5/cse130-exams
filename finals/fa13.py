# 3a
def lookup(d, k):
    return [v for l, v in d if l == k]

d = [("a", 10), ("b", 20), ("c", 30), ("a", 40)]
assert(lookup(d, "a") == [10, 40])
assert(lookup(d, "b") == [20])
assert(lookup(d, "c") == [30])
assert(lookup(d, "d") == [])

# 3b
def cond(b, t, f):
    if b:
        return t
    else:
        return f

def update(d, k, v):
    return [(l, cond(l == k, v, w)) for l, w in d]

d = [("a", 10), ("b", 20), ("c", 30), ("a", 40)]
assert(update(d, "a", "CSE130") ==
       [('a', 'CSE130'), ('b', 20), ('c', 30), ('a', 'CSE130')])
assert(update(d, "b", "CSE130") ==
       [('a', 10), ('b', 'CSE130'), ('c', 30), ('a', 40)])
assert(update(d, "d", "CSE130") ==
       [('a', 10), ('b', 20), ('c', 30), ('a', 40)])

# 3c
def delete(d, k):
    return [(l, v) for l, v in d if l != k]

d = [("a", 10), ("b", 20), ("c", 30), ("a", 40)]
assert(delete(d, "a") == [("b", 20), ("c", 30)])

# 3d
def add(d, k, v):
    return d + [(k, v)]

d = [("a", 10), ("b", 20), ("c", 30), ("a", 40)]
assert(add(d, "d", 5) ==
       [("a", 10), ("b", 20), ("c", 30), ("a", 40), ("d", 5)])

# 3e
def update(d, k, v):
    return map(lambda (l, w): (l, v if l == k else w), d)

d = [("a", 10), ("b", 20), ("c", 30), ("a", 40)]
assert(update(d, "a", "CSE130") ==
       [('a', 'CSE130'), ('b', 20), ('c', 30), ('a', 'CSE130')])
assert(update(d, "b", "CSE130") ==
       [('a', 10), ('b', 'CSE130'), ('c', 30), ('a', 40)])
assert(update(d, "d", "CSE130") ==
       [('a', 10), ('b', 20), ('c', 30), ('a', 40)])

# 4
def in_range(i, (lo, hi)):
    def decorator(f):
        def decorated(*args):
            if 0 <= i < len(args) and not lo <= args[i] <= hi:
                raise Exception("%dth arg %d too %s" % (i, args[i],
                                "small" if args[i] < lo else "big"))
            ret = f(*args)
            if i == -1 and not lo <= ret <= hi:
                raise Exception("Return value %d too %s" % (ret,
                                "small" if args[i] < lo else "big"))
            return ret
        return decorated
    return decorator

# Need to test manually because of exceptions
