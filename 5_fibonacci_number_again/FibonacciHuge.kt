/*
 * Author: Jehadur Rahman
 * Organization: CyArm
 * GitHub: https://github.com/JehadurRE
 * Website: jehadurre.me
 * Date: 2025-07-10 09:43 (Thu)
 */

import java.util.*

fun getFibonacciHugeNaive(n: Long, m: Long): Long {
    if (n <= 1) return n

    var previous: Long = 0
    var current: Long = 1

    for (i in 0 until n - 1) {
        val tmpPrevious = previous
        previous = current
        current += tmpPrevious
    }

    return current % m
}

fun findPisanoPeriod(m: Long): Long {
    var prev: Long = 0
    var current: Long = 1

    for (i in 0 until m * m) {
        val temp = current
        current = (prev + temp) % m
        prev = temp

        if (prev == 0L && current == 1L) {
            return i + 1
        }
    }
    return 0
}

fun getFibonacciHugeFast(n: Long, m: Long): Long {
    val p = findPisanoPeriod(m)
    val newN = n % p

    if (newN <= 1) return newN

    var previous: Long = 0
    var current: Long = 1

    for (i in 0 until newN - 1) {
        val tmpPrevious = previous
        previous = current
        current = (tmpPrevious + current) % m
    }

    return current
}

fun main(args: Array<String>) {
    val scanner = Scanner(System.`in`)
    val n = scanner.nextLong()
    val m = scanner.nextLong()
    // println(getFibonacciHugeNaive(n, m))
    println(getFibonacciHugeFast(n, m))
}