import numpy as np 
from math import sqrt


def erat(num): 
    l = [ x for x in range(2, num) ]
    p = []

    while ((newPrime := l.pop(0)) < sqrt(num)) : 
        p.append(newPrime)
        l = list(filter(lambda x: (x % newPrime) != 0, l))

    return p + l

def main(): 
    print(erat(10000000))


main()
