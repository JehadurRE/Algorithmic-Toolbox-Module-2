"""
Author: Jehadur Rahman
Organization: CyArm
GitHub: https://github.com/JehadurRE
Website: jehadurre.me
Date: 2025-07-10 09:43 (Thu)
"""

def gcd_naive(a, b):
    current_gcd = 1
    for d in range(2, min(a, b) + 1):
        if a % d == 0 and b % d == 0:
            if d > current_gcd:
                current_gcd = d
    return current_gcd

def gcd_fast(a, b):
    if b == 0:
        return a
    a = a % b
    return gcd_fast(b, a)

def test_solution():
    for a in range(1, 20):
        for b in range(1, 20):
            assert gcd_fast(a, b) == gcd_naive(a, b)
    print("All tests passed!")

if __name__ == "__main__":
    # test_solution()
    a, b = map(int, input().split())
    print(gcd_fast(a, b))
