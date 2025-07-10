"""
Author: Jehadur Rahman
Organization: CyArm
GitHub: https://github.com/JehadurRE
Website: jehadurre.me
Date: 2025-07-10 09:43 (Thu)
"""

def fibonacci_naive(n):
    if n <= 1:
        return n
    return fibonacci_naive(n - 1) + fibonacci_naive(n - 2)

def fibonacci_fast(n):
    if n <= 1:
        return n
    
    arr = [0] * (n + 1)
    arr[0] = 0
    arr[1] = 1
    
    for i in range(2, n + 1):
        arr[i] = arr[i - 1] + arr[i - 2]
    
    return arr[n]

def test_solution():
    assert fibonacci_fast(3) == 2
    assert fibonacci_fast(10) == 55
    for n in range(20):
        assert fibonacci_fast(n) == fibonacci_naive(n)

if __name__ == '__main__':
    input_n = int(input())
    # print(fibonacci_naive(input_n))  # Slow version
    # test_solution()  # Test the implementation
    print(fibonacci_fast(input_n))
