/*
 * Author: Jehadur Rahman
 * Organization: CyArm
 * GitHub: https://github.com/JehadurRE
 * Website: jehadurre.me
 * Date: 2025-07-10 09:43 (Thu)
 */

#include <iostream>
#include<cassert>
inline long long lcm_naive(int a, int b) {
  for (long l = 1; l <= (long long) a * b; ++l)
    if (l % a == 0 && l % b == 0)
      return l;

  return (long long) a * b;
}


inline long long gcd (long long a , long long b)
{
  while (b !=0)
  {
    long long temp = b ;
    b = a % b;
    a = temp;
  }
  return a;
}

inline long long lcm_fast(int a , int b )
{
  return (long long )(a / gcd(a, b))*b;
}

void test_solution()
{
  for (int a = 1; a < 20; ++a)
  {
    for (int b = 1; b < 20; ++b)
    {
      assert(lcm_fast(a, b) == lcm_naive(a, b));
    }
  }
  std::cout << "All tests passed!\n";
}
int main() {
  // test_solution();
  int a, b;
  std::cin >> a >> b;
  std::cout << lcm_fast(a, b) << std::endl;
  return 0;
}
