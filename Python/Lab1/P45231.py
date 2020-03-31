def fibs():
    yield 0
    a,b = 0,1
    while True:
        a,b = b, a+b
        yield a

# TESTING
# fib = fibs()
# print([next(fib) for n in range(10)])


def roots(n):
    curr = n
    yield curr
    while True:
        prev = curr
        curr = 0.5 * (prev + (n/prev))
        yield curr

# TESTING
# g2 = roots(4)
# print([round(next(g2), 10) for n in range(10)])


def isPrime(x):
    # Para todo x > 2
    for d in range(2, x//2 + 1, 1):
        if (x%d == 0):
            return False
    return True

def primes():
    yield 2
    p = 3
    while True:
        if (isPrime(p)):
            yield p
        p += 1


# TESTING
# g3 = primes()
# print([next(g3) for n in range(20)])

def is_hamming_numbers(x):
	if x == 1:
		return True
	if x % 2 == 0:
		return is_hamming_numbers(x/2)
	if x % 3 == 0:
		return is_hamming_numbers(x/3)
	if x % 5 == 0:
		return is_hamming_numbers(x/5)
	return False

def hammings():
    n = 1
    while True:
        if (is_hamming_numbers(n)):
            yield n
        n += 1


# TESTING
# g4 = hammings()
# print([next(g4) for n in range(20)])