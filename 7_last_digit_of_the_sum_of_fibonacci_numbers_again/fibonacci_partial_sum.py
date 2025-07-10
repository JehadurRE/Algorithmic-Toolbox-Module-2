"""
Author: Jehadur Rahman
Organization: CyArm
GitHub: https://github.com/JehadurRE
Website: jehadurre.me
Date: 2025-07-10 09:43 (Thu)
"""

# Uses python3
import sys

def fibonacci_partial_sum_naive(from_, to):
    _sum = 0

    current = 0
    _next = 1

    for i in range(to + 1):
        if i >= from_:
            _sum += current

        current, _next = _next, current + _next

    return _sum % 10

def fibonacci_partial_sum_fast(from_, to):
    sum_val = 0
    
    current = 0
    next_val = 1
    from_ = from_ % 60
    to = to % 60
    
    if to < from_:
        to += 60
    
    for i in range(to + 1):
        if i >= from_:
            sum_val += current
        
        new_current = next_val
        next_val = (next_val + current) % 10
        current = new_current
    
    return sum_val % 10

if __name__ == '__main__':
    input_data = sys.stdin.read()
    from_, to = map(int, input_data.split())
    print(fibonacci_partial_sum_fast(from_, to))
