/*
 * Author: Jehadur Rahman
 * Organization: CyArm
 * GitHub: https://github.com/JehadurRE
 * Website: jehadurre.me
 * Date: 2025-07-10 09:43 (Thu)
 */

import java.util.*

fun getFibonacciSumNaive(n: Long): Long {
    if (n <= 1)
        return n

    var previous: Long = 0
    var current: Long = 1
    var sum: Long = 1

    for (i in 0 until n - 1) {
        val tmpPrevious = previous
        previous = current
        current += tmpPrevious
        sum += current
    }

    return sum % 10
}

fun fibonacciSumFast(n: Long): Int {
    val newN = n % 60 // Pisano period for mod 10

    if (newN <= 1)
        return newN.toInt()

    var previous: Long = 0
    var current: Long = 1
    var sum: Long = 1

    for (i in 0 until newN - 1) {
        val tmpPrevious = previous
        previous = current
        current = (tmpPrevious + current) % 10
        sum = (sum + current) % 10
    }

    return sum.toInt()
}

fun main(args: Array<String>) {
    val scanner = Scanner(System.`in`)
    val n = scanner.nextLong()
    // val s = getFibonacciSumNaive(n)
    val s = fibonacciSumFast(n)
    println(s)
}