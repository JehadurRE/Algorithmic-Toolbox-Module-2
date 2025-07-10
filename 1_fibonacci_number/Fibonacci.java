/*
 * Author: Jehadur Rahman
 * Organization: CyArm
 * GitHub: https://github.com/JehadurRE
 * Website: jehadurre.me
 * Date: 2025-07-10 09:43 (Thu)
 */

import java.util.Scanner;

public class Fibonacci {
  private static long calc_fib_naive(int n) {
    if (n <= 1)
      return n;

    return calc_fib_naive(n - 1) + calc_fib_naive(n - 2);
  }

  private static long calc_fib_fast(int n) {
    if (n <= 1)
      return n;

    long[] arr = new long[n + 1];
    arr[0] = 0;
    arr[1] = 1;

    for (int i = 2; i <= n; i++) {
      arr[i] = arr[i - 1] + arr[i - 2];
    }

    return arr[n];
  }

  private static void test_solution() {
    assert calc_fib_fast(3) == 2;
    assert calc_fib_fast(10) == 55;
    for (int n = 0; n < 20; n++) {
      assert calc_fib_fast(n) == calc_fib_naive(n);
    }
  }

  public static void main(String args[]) {
    Scanner in = new Scanner(System.in);
    int n = in.nextInt();

    // System.out.println(calc_fib_naive(n));  // Slow version
    // test_solution();  // Test the implementation
    System.out.println(calc_fib_fast(n));
  }
}
