package d04

import utils.memoize
import utils.readFile
import kotlin.system.measureTimeMillis


val input = {
    readFile("../input/d04.txt").readText().trim()
}.memoize()

fun parseInput(s: String): Pair<Int, Int> {
    val startEnd = s.split("-").mapTo(mutableListOf(), { it.toInt() })
    return startEnd[0] to startEnd[1]
}


fun Char.toInteger(): Int {
    val zero = '0'.toInt()
    return this.toInt() - zero
}


fun validPass(password: Int): Boolean {
    val p = password.toString()
    var hasAdjacent = false

    var prev = p[0].toInteger()
    for (c in p.subSequence(1 until p.length)) {
        val cInt = c.toInteger()
        if (cInt < prev) {
            return false
        }
        if (cInt == prev) {
            hasAdjacent = true
        }
        prev = cInt
    }

    return hasAdjacent
}


fun validPass2(password: Int): Boolean {
    val p = password.toString()
    var hasAdjacent = false

    var prev = p[0].toInteger()
    var count = 1
    for (i in 1 until p.length) {
        val c = p[i].toInteger()
        if (c < prev) {
            return false
        }

        if (c == prev) {
            count++
        }

        if (c > prev || i == p.length - 1) {
            if (count == 2) {
                hasAdjacent = true
            }
            prev = c
            count = 1
        }
    }
    return hasAdjacent
}


fun passwordCount(start: Int, end: Int, check: (Int) -> Boolean): Int {
    return (start until end).filter { check(it) }.count()
}


fun part1() {
    val (start, end) = parseInput(input())
    val count = passwordCount(start, end, ::validPass)
    println("part1: $count")
}


fun part2() {
    val (start, end) = parseInput(input())
    val count = passwordCount(start, end, ::validPass2)
    println("part2: $count")
}


fun all() {
    println("\n** Day 4 **")
    val ms1 = measureTimeMillis { part1() }
    println("---> ${ms1}ms")
    val ms2 = measureTimeMillis { part2() }
    println("---> ${ms2}ms")
}