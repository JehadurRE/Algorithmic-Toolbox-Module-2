"""
Author: Jehadur Rahman
Organization: CyArm
GitHub: https://github.com/JehadurRE
Website: jehadurre.me
Date: 2025-07-10 09:43 (Thu)
"""

def fibonacci_huge_naive(n, m):
    if n <= 1:
        return n

    previous = 0
    current = 1

    for _ in range(n - 1):
        previous, current = current, previous + current

    return current % m

def find_pisano_period(m):
    prev = 0
    current = 1
    
    for i in range(m * m):
        temp = current
        current = (prev + temp) % m
        prev = temp
        
        if prev == 0 and current == 1:
            return i + 1
    return 0

def fibonacci_huge_fast(n, m):
    p = find_pisano_period(m)
    n = n % p
    
    if n <= 1:
        return n
    
    previous = 0
    current = 1
    
    for i in range(n - 1):
        tmp_previous = previous
        previous = current
        current = (tmp_previous + current) % m
    
    return current

if __name__ == '__main__':
    n, m = map(int, input().split())
    # print(fibonacci_huge_naive(n, m))
    print(fibonacci_huge_fast(n, m))
