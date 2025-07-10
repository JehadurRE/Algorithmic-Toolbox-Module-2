/*
 * Author: Jehadur Rahman
 * Organization: CyArm
 * GitHub: https://github.com/JehadurRE
 * Website: jehadurre.me
 * Date: 2025-07-10 09:43 (Thu)
 */

import java.util.*

fun gcdNaive(a: Int, b: Int): Int {
    var currentGcd = 1
    var d = 2
    while (d <= a && d <= b) {
        if (a % d == 0 && b % d == 0) {
            if (d > currentGcd) {
                currentGcd = d
            }
        }
        ++d
    }
    return currentGcd
}

fun gcdFast(a: Int, b: Int): Int {
    if (b == 0) return a
    val newA = a % b
    return gcdFast(b, newA)
}

fun testSolution() {
    for (a in 1..19) {
        for (b in 1..19) {
            assert(gcdFast(a, b) == gcdNaive(a, b))
        }
    }
    println("All tests passed!")
}

fun main(args: Array<String>) {
    // testSolution()
    val scanner = Scanner(System.`in`)
    val a = scanner.nextInt()
    val b = scanner.nextInt()

    println(gcdFast(a, b))
}