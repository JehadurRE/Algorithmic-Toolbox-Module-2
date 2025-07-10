/*
 * Author: Jehadur Rahman
 * Organization: CyArm
 * GitHub: https://github.com/JehadurRE
 * Website: jehadurre.me
 * Date: 2025-07-10 09:43 (Thu)
 */

#include <iostream>

long long get_fibonacci_huge_naive(long long n, long long m) {
    if (n <= 1)
        return n;

    long long previous = 0;
    long long current  = 1;

    for (long long i = 0; i < n - 1; ++i) {
        long long tmp_previous = previous;
        previous = current;
        current = (tmp_previous + current);
    }

    return current % m;
}


long long find_pisano_period(long long m)
{
   long long prev =0 ;
   long long current = 1;

   for ( long long i = 0 ; i<m*m;i++)
   {
       long long temp = current;
       current = (prev + temp)%m;
       prev = temp;


       if (prev ==0 && current ==1)
       {
           return i + 1;
       }
   }
   return 0;
}

long long get_fibonacci_huge_fast(long long n, long long m)
{

    long long p = find_pisano_period(m);
    n = n % p;
    if (n <= 1)
        return n;

    long long previous = 0;
    long long current = 1;

    for (long long i = 0; i < n - 1; ++i)
    {
        long long tmp_previous = previous;
        previous = current;
        current = (tmp_previous + current)%m;
    }

    return current;
}

int main() {
    long long n, m;
    std::cin >> n >> m;
    // std::cout << get_fibonacci_huge_naive(n, m) << '\n';
    std::cout << get_fibonacci_huge_fast(n, m) << '\n';
}
