/*
 * Author: Jehadur Rahman
 * Organization: CyArm
 * GitHub: https://github.com/JehadurRE
 * Website: jehadurre.me
 * Date: 2025-07-10 09:43 (Thu)
 */

import java.util.*

fun getFibonacciLastDigit(n: Int): Int {
    if (n <= 1)
        return n

    var previous = 0
    var current = 1

    for (i in 0 until n - 1) {
        val tmpPrevious = previous
        previous = current
        current = (tmpPrevious + current) % 10
    }

    return current
}

fun main(args: Array<String>) {
    val scanner = Scanner(System.`in`)
    val n = scanner.nextInt()
    val c = getFibonacciLastDigit(n)
    println(c)
}