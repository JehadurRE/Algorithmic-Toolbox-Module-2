"""
Author: Jehadur Rahman
Organization: CyArm
GitHub: https://github.com/JehadurRE
Website: jehadurre.me
Date: 2025-07-10 09:43 (Thu)
"""

def lcm_naive(a, b):
    for l in range(1, a * b + 1):
        if l % a == 0 and l % b == 0:
            return l
    assert False

def gcd(a, b):
    while b != 0:
        temp = b
        b = a % b
        a = temp
    return a

def lcm_fast(a, b):
    return (a // gcd(a, b)) * b

def test_solution():
    for a in range(1, 20):
        for b in range(1, 20):
            assert lcm_fast(a, b) == lcm_naive(a, b)
    print("All tests passed!")

if __name__ == '__main__':
    # test_solution()
    a, b = map(int, input().split())
    print(lcm_fast(a, b))

