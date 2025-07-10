/*
 * Author: Jehadur Rahman
 * Organization: CyArm
 * GitHub: https://github.com/JehadurRE
 * Website: jehadurre.me
 * Date: 2025-07-10 09:43 (Thu)
 */

#include <iostream>
#include <cassert>

int gcd_naive(int a, int b) {
  int current_gcd = 1;
  for (int d = 2; d <= a && d <= b; d++) {
    if (a % d == 0 && b % d == 0) {
      if (d > current_gcd) {
        current_gcd = d;
      }
    }
  }
  return current_gcd;
}

int gcd_fast(int a , int b)
{
  if (b
    == 0) return a;
  a = a % b;
  return gcd_fast(b, a);

}

void test_solution()
{
  for (int a = 1; a < 20; ++a)
  {
    for (int b = 1; b < 20; ++b)
    {
      assert(gcd_fast(a, b) == gcd_naive(a, b));
    }
  }
  std::cout << "All tests passed!\n";
}

int main() {
  // test_solution();
  int a, b;
  std::cin >> a >> b;
  std::cout << gcd_fast(a, b) << std::endl;
  return 0;
}
