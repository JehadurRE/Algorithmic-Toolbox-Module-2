/*
 * Author: Jehadur Rahman
 * Organization: CyArm
 * GitHub: https://github.com/JehadurRE
 * Website: jehadurre.me
 * Date: 2025-07-10 09:43 (Thu)
 */

import java.util.*;

public class FibonacciHuge {
    private static long getFibonacciHugeNaive(long n, long m) {
        if (n <= 1)
            return n;

        long previous = 0;
        long current = 1;

        for (long i = 0; i < n - 1; ++i) {
            long tmp_previous = previous;
            previous = current;
            current = tmp_previous + current;
        }

        return current % m;
    }

    private static long findPisanoPeriod(long m) {
        long prev = 0;
        long current = 1;

        for (long i = 0; i < m * m; i++) {
            long temp = current;
            current = (prev + temp) % m;
            prev = temp;

            if (prev == 0 && current == 1) {
                return i + 1;
            }
        }
        return 0;
    }

    private static long getFibonacciHugeFast(long n, long m) {
        long p = findPisanoPeriod(m);
        n = n % p;

        if (n <= 1)
            return n;

        long previous = 0;
        long current = 1;

        for (long i = 0; i < n - 1; ++i) {
            long tmp_previous = previous;
            previous = current;
            current = (tmp_previous + current) % m;
        }

        return current;
    }
    
    public static void main(String[] args) {
        Scanner scanner = new Scanner(System.in);
        long n = scanner.nextLong();
        long m = scanner.nextLong();
        // System.out.println(getFibonacciHugeNaive(n, m));
        System.out.println(getFibonacciHugeFast(n, m));
    }
}

