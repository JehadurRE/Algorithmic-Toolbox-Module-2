/*
 * Author: Jehadur Rahman
 * Organization: CyArm
 * GitHub: https://github.com/JehadurRE
 * Website: jehadurre.me
 * Date: 2025-07-10 09:43 (Thu)
 */

#include <iostream>

int fibonacci_sum_naive(long long n) {

    if (n <= 1)
        return n;

    long long previous = 0;
    long long current  = 1;
    long long sum      = 1;

    for (long long i = 0; i < n - 1; ++i) {
        long long tmp_previous = previous;
        previous = current;
        current = tmp_previous + current;
        sum += current;
    }

    return sum % 10;
}

int fibonacci_sum_fast(long long n)
{

    n = n % 60; 

    if (n <= 1)
        return n;

     // Pisano period for mod 10

    long long previous = 0;
    long long current = 1;
    long long sum = 1;

    for (long long i = 0; i < n - 1; ++i)
    {
        long long tmp_previous = previous;
        previous = current;
        current = (tmp_previous + current) % 10; // ✅ Fix: modulo here!
        sum = (sum + current) % 10;              // ✅ Keep sum mod 10
    }

    return sum; // ✅ Already modded
}

int main() {
    long long n = 0;
    std::cin >> n;
    // std::cout << fibonacci_sum_naive(n);
    std::cout << fibonacci_sum_fast(n);
}
