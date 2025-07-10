"""
Author: Jehadur Rahman
Organization: CyArm
GitHub: https://github.com/JehadurRE
Website: jehadurre.me
Date: 2025-07-10 09:43 (Thu)
"""

def fibonacci_sum_naive(n):
    if n <= 1:
        return n

    previous, current, _sum = 0, 1, 1

    for _ in range(n - 1):
        previous, current = current, previous + current
        _sum += current

    return _sum % 10

def fibonacci_sum_fast(n):
    n = n % 60  # Pisano period for mod 10
    
    if n <= 1:
        return n
    
    previous = 0
    current = 1
    sum_val = 1
    
    for i in range(n - 1):
        tmp_previous = previous
        previous = current
        current = (tmp_previous + current) % 10
        sum_val = (sum_val + current) % 10
    
    return sum_val

if __name__ == '__main__':
    n = int(input())
    # print(fibonacci_sum_naive(n))
    print(fibonacci_sum_fast(n))
