def myLength(L):
    # count = 0
    # while(not [] == L):
    #     L.pop()
    #     count += 1
    
    return len(L)

# TESTING:
# L = [1,2,3,4,5,6,7,8,9,0]
# print(len(L))
# print(myLength(L))

def myMaximum(L):
    maximum = L[0]
    for x in L:
        if maximum < x:
           maximum = x

    return maximum 

# TESTING:
# L = [1,2,3,4,5,6,7,8,9,0]
# print(max(L))
# print(myMaximum(L))

def average(L):
    return sum(L)/len(L)

# TESTING:
# L = [1,2,3,4,5,6,7,8,9,10]
# print(average(L))
# print(average([1,2,3]))

def buildPalindrome(L):
    l = len(L)
    if l == 0:
        return []
    
    R = []
    for i in range(l-1,-1,-1):
        R.append(L[i])

    return R + L

# TESTING:
# L = [1,2,3,4,5,6,7,8,9,10]
# print(buildPalindrome(L))
# print(buildPalindrome(L))
# print(buildPalindrome(['pa','amb','oli']))

def remove(L1, L2):
    S = set(L2)
    R = []
    for x in L1:
        if x not in S:
            R.append(x)
    return R
    
# TESTING:
# L = [1,2,3,4,5,6,7,8,9,10]
# print(remove(L,[3,6,7,1]))

def flatten(L):
    R = []
    for x in L:
        if isinstance(x,list):
            R += flatten(x)
        else:
            R.append(x)
    
    return R

# TESTING:
# print(flatten([[2,6],[[8,1,4],[3,'uau']],[[],[1]],[[]]]))

def oddsNevens(L):
    even = []
    odd = []
    for x in L:
        if x%2 == 0:
            even.append(x)
        else:
            odd.append(x)
    return (odd,even)

# TESTING:
# print(oddsNevens([1,4,5,3,4,5,1,2,7,4,2]))

def isPrime(x):
    if x < 2:
        return False

    for d in range(2, x, 1):
        if (x%d == 0):
            return False

    return True

def primeDivisors(n):

    # Lista de todos los divisores entre 2
    divisors = [ d for d in range(2,1 + n//2) if n%d == 0 ] + [n]
    return [ p for p in divisors if isPrime(p)]
        
# TESTING:
# print(primeDivisors(255))