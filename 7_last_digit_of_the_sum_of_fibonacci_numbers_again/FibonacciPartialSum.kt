/*
 * Author: Jehadur Rahman
 * Organization: CyArm
 * GitHub: https://github.com/JehadurRE
 * Website: jehadurre.me
 * Date: 2025-07-10 09:43 (Thu)
 */

import java.util.*

fun getFibonacciPartialSumNaive(from: Long, to: Long): Long {
    var sum: Long = 0

    var current: Long = 0
    var next: Long = 1

    for (i in 0..to) {
        if (i >= from) {
            sum += current
        }

        val newCurrent = next
        next += current
        current = newCurrent
    }

    return sum % 10
}

fun getFibonacciPartialSumFast(from: Long, to: Long): Long {
    var sum: Long = 0

    var current: Long = 0
    var next: Long = 1
    var newFrom = from % 60
    var newTo = to % 60

    if (newTo < newFrom) {
        newTo += 60
    }

    for (i in 0..newTo) {
        if (i >= newFrom) {
            sum += current
        }

        val newCurrent = next
        next = (next + current) % 10
        current = newCurrent
    }

    return sum % 10
}

fun main(args: Array<String>) {
    val scanner = Scanner(System.`in`)
    val from = scanner.nextLong()
    val to = scanner.nextLong()
    // println(getFibonacciPartialSumNaive(from, to))
    println(getFibonacciPartialSumFast(from, to))
}