/*
 * Author: Jehadur Rahman
 * Organization: CyArm
 * GitHub: https://github.com/JehadurRE
 * Website: jehadurre.me
 * Date: 2025-07-10 09:43 (Thu)
 */

import java.util.*

fun calcFibNaive(n: Long): Long {
    return if (n <= 1) n else calcFibNaive(n - 1) + calcFibNaive(n - 2)
}

fun calcFib(n: Long): Long {
    if (n <= 1) return n
    
    val arr = LongArray(n.toInt() + 1)
    arr[0] = 0
    arr[1] = 1
    
    for (i in 2..n.toInt()) {
        arr[i] = arr[i - 1] + arr[i - 2]
    }
    
    return arr[n.toInt()]
}

fun testSolution() {
    assert(calcFib(3) == 2L)
    assert(calcFib(10) == 55L)
    for (n in 0L..19L) {
        assert(calcFib(n) == calcFibNaive(n))
    }
}

fun main(args: Array<String>) {
    val scanner = Scanner(System.`in`)
    val n = scanner.nextLong()

    // println(calcFibNaive(n))  // Slow version
    // testSolution()  // Test the implementation
    println(calcFib(n))
}