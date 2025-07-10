/*
 * Author: Jehadur Rahman
 * Organization: CyArm
 * GitHub: https://github.com/JehadurRE
 * Website: jehadurre.me
 * Date: 2025-07-10 09:43 (Thu)
 */

import java.util.*;

public class GCD {
  private static int gcd_naive(int a, int b) {
    int current_gcd = 1;
    for(int d = 2; d <= a && d <= b; ++d) {
      if (a % d == 0 && b % d == 0) {
        if (d > current_gcd) {
          current_gcd = d;
        }
      }
    }
    return current_gcd;
  }

  private static int gcd_fast(int a, int b) {
    if (b == 0) return a;
    a = a % b;
    return gcd_fast(b, a);
  }

  private static void test_solution() {
    for (int a = 1; a < 20; ++a) {
      for (int b = 1; b < 20; ++b) {
        assert gcd_fast(a, b) == gcd_naive(a, b);
      }
    }
    System.out.println("All tests passed!");
  }

  public static void main(String args[]) {
    // test_solution();
    Scanner scanner = new Scanner(System.in);
    int a = scanner.nextInt();
    int b = scanner.nextInt();

    System.out.println(gcd_fast(a, b));
  }
}
