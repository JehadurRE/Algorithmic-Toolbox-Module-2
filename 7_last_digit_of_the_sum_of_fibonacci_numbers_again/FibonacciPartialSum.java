/*
 * Author: Jehadur Rahman
 * Organization: CyArm
 * GitHub: https://github.com/JehadurRE
 * Website: jehadurre.me
 * Date: 2025-07-10 09:43 (Thu)
 */

import java.util.*;

public class FibonacciPartialSum {
    private static long getFibonacciPartialSumNaive(long from, long to) {
        long sum = 0;

        long current = 0;
        long next = 1;

        for (long i = 0; i <= to; ++i) {
            if (i >= from) {
                sum += current;
            }

            long new_current = next;
            next = next + current;
            current = new_current;
        }

        return sum % 10;
    }

    private static long getFibonacciPartialSumFast(long from, long to) {
        long sum = 0;

        long current = 0;
        long next = 1;
        from = from % 60;
        to = to % 60;

        if (to < from) {
            to += 60;
        }

        for (long i = 0; i <= to; ++i) {
            if (i >= from) {
                sum += current;
            }

            long new_current = next;
            next = (next + current) % 10;
            current = new_current;
        }

        return sum % 10;
    }
    
    public static void main(String[] args) {
        Scanner scanner = new Scanner(System.in);
        long from = scanner.nextLong();
        long to = scanner.nextLong();
        // System.out.println(getFibonacciPartialSumNaive(from, to));
        System.out.println(getFibonacciPartialSumFast(from, to));
    }
}

