def absValue(x):
    return abs(x)

# print("Valor Absoluto:")
# print(absValue(-1))
# print(absValue(1))
# print(absValue(-21312))
# print(absValue(21312))

def power(x,p):

    if p == 0:
        return 1
    
    res = x
    for _ in range(1,p):
        res *= x
    
    return res

# print("\n")
# print("Power:")
# print(power(2,0))
# print(power(2,1))
# print(power(2,2))
# print(power(2,3))
# print(power(2,4))

def isPrime(x):

    if x < 2:
        return False


    for d in range(2, x, 1):
        if (x%d == 0):
            return False
    
    return True

# print("\n")
# print("Prime:")
# for d in range(1, 50):
#     print(d, "es primo:", isPrime(d))

def slowFib(n):

    if n < 2: return n
    
    return slowFib(n-1) + slowFib(n-2)

# print("\n")
# print("Slowfib:")
# for d in range(1, 32):
#     print(d, "-->", slowFib(d))



def rec_quickFib(fib, n):

    if (fib[n] != -1):
        return fib[n]

    
    fib[n-1] = rec_quickFib(fib, n-1)
    fib[n] = fib[n-1] + fib[n-2]
    return fib[n]
        


def quickFib(n):

    if n < 2: return n

    fib = [-1 for _ in range(n+1)]
    fib[0] = 0
    fib[1] = 1

    return rec_quickFib(fib, n)

# print("\n")
# print("QuickFib:")
# for d in range(1, 200):
#     print(d, "-->", quickFib(d))