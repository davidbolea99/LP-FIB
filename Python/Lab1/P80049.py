def count_unique(L):
    return len(set(L))

# TESTING
# L = [1, 3, 2, 2, 3, 4]
# print(count_unique(L))


def remove_duplicates(L):
    return list(set(L))

# TESTING
# L = [3, 1, 3, 2, 3, 2, 3, 4]
# print(remove_duplicates(L))


def flatten(L):
    return [x for sublista in L for x in sublista]

# TESTING
# L = [[2, 6], [8, 1, 4], [], [1]]
# print(flatten(L))


def flatten_rec(L):
    return sum( ([x] if not isinstance(x,list) else flatten_rec(x) for x in L), [] )
     
# TESTING
# L = [3, [1], [], [4, [], [5, 3]], [2, 1]]
# print(flatten_rec(L))
