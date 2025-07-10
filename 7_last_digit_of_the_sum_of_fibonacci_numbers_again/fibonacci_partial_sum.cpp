/*
 * Author: Jehadur Rahman
 * Organization: CyArm
 * GitHub: https://github.com/JehadurRE
 * Website: jehadurre.me
 * Date: 2025-07-10 09:43 (Thu)
 */

#include <iostream>
#include <vector>
using std::vector;

long long get_fibonacci_partial_sum_naive(long long from, long long to) {
    long long sum = 0;

    long long current = 0;
    long long next  = 1;

    for (long long i = 0; i <= to; ++i) {
        if (i >= from) {
            sum += current;
        }

        long long new_current = next;
        next = next + current;
        current = new_current;
    }

    return sum % 10;
}

long long get_fibonacci_partial_sum_fast(long long from, long long to)
{
    long long sum = 0;

    long long current = 0;
    long long next = 1;
    from = from % 60;
    to = to % 60;

    if (to<from )
    {
        to += 60;
    }
    for (long long i = 0; i <= to; ++i)
    {
        if (i >= from)
        {
            sum += current;
        }

        long long new_current = next;
        next = (next + current)%10;
        current = new_current;
    }

    return sum %10;
}
int main() {
    long long from, to;
    std::cin >> from >> to;
    std::cout << get_fibonacci_partial_sum_fast(from, to) << '\n';
}
