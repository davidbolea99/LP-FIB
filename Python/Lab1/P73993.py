from functools import reduce

def evens_product(L):
    multiplicador = lambda acc,y: acc * y if y%2 == 0 else acc
    return reduce(multiplicador, L, 1)

# TESTING
# print(evens_product([1,2,4,3]))


def reverse(L):
    rev_concat = lambda acc,x: [x] + acc
    return reduce(rev_concat, L, [])
    
# TESTING
# print(reverse([1,2,3]))



def zip_with(f, L1, L2):
    z = list(zip(L1,L2))
    F = lambda t: f(t[0],t[1])
    return list(map(F, z))

# TESTING
# print(zip_with(lambda x, y: x * y, [1, 2, 3], [10, 2]))


def count_if(f, L):
    return reduce((lambda acc, x: acc + 1 if f(x) else acc),L,0)

# TESTING
# print(count_if(lambda x: x % 2 == 0, [1, 2, 3, 4, 5]))