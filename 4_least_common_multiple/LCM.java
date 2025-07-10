/*
 * Author: Jehadur Rahman
 * Organization: CyArm
 * GitHub: https://github.com/JehadurRE
 * Website: jehadurre.me
 * Date: 2025-07-10 09:43 (Thu)
 */

import java.util.*;

public class LCM {
  private static long lcm_naive(int a, int b) {
    for (long l = 1; l <= (long) a * b; ++l)
      if (l % a == 0 && l % b == 0)
        return l;
    return (long) a * b;
  }

  private static long gcd(long a, long b) {
    while (b != 0) {
      long temp = b;
      b = a % b;
      a = temp;
    }
    return a;
  }

  private static long lcm_fast(int a, int b) {
    return (long)(a / gcd(a, b)) * b;
  }

  private static void test_solution() {
    for (int a = 1; a < 20; ++a) {
      for (int b = 1; b < 20; ++b) {
        assert lcm_fast(a, b) == lcm_naive(a, b);
      }
    }
    System.out.println("All tests passed!");
  }

  public static void main(String args[]) {
    // test_solution();
    Scanner scanner = new Scanner(System.in);
    int a = scanner.nextInt();
    int b = scanner.nextInt();

    System.out.println(lcm_fast(a, b));
  }
}
