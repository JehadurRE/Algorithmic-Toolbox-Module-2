/*
 * Author: Jehadur Rahman
 * Organization: CyArm
 * GitHub: https://github.com/JehadurRE
 * Website: jehadurre.me
 * Date: 2025-07-10 09:43 (Thu)
 */

import java.util.*

fun lcmNaive(a: Int, b: Int): Long {
    for (l in 1..a.toLong() * b)
        if (l % a == 0L && l % b == 0L)
            return l
    return a.toLong() * b
}

fun gcd(a: Long, b: Long): Long {
    var x = a
    var y = b
    while (y != 0L) {
        val temp = y
        y = x % y
        x = temp
    }
    return x
}

fun lcmFast(a: Int, b: Int): Long {
    return (a.toLong() / gcd(a.toLong(), b.toLong())) * b.toLong()
}

fun testSolution() {
    for (a in 1..19) {
        for (b in 1..19) {
            assert(lcmFast(a, b) == lcmNaive(a, b))
        }
    }
    println("All tests passed!")
}

fun main(args: Array<String>) {
    // testSolution()
    val scanner = Scanner(System.`in`)
    val a = scanner.nextInt()
    val b = scanner.nextInt()

    println(lcmFast(a, b))
}