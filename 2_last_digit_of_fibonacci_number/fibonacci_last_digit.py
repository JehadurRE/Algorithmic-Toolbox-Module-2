"""
Author: Jehadur Rahman
Organization: CyArm
GitHub: https://github.com/JehadurRE
Website: jehadurre.me
Date: 2025-07-10 09:43 (Thu)
"""

def fibonacci_last_digit(n):
    if n <= 1:
        return n

    previous = 0
    current = 1

    for i in range(n - 1):
        tmp_previous = previous
        previous = current
        current = (tmp_previous + current) % 10

    return current


if __name__ == '__main__':
    n = int(input())
    print(fibonacci_last_digit(n))
