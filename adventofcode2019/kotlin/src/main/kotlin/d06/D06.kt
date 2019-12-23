package d06

import utils.memoize
import utils.readFile
import kotlin.system.measureTimeMillis


val input = {
    readFile("../input/d06.txt").readText()
}.memoize()


fun parse(s: String): Map<String, String> {
    return s.trim()
            .lines()
            .associateTo(mutableMapOf<String, String>(), {
                val (center, orbiter) = it.split(")")
                orbiter to center
            })
}

fun countOrbits(input: String): Int {
    val map = parse(input)
    var count = 0
    for ((k, v) in map.entries) {
        var next: String? = k
        while (true) {
            next = map.get(next)
            if (next == null) { break }
            count++
        }
    }
    return count
}

fun part1() {
    val count = countOrbits(input())
    println("part1: $count")
}


fun parentsOf(map: Map<String, String>, start: String): List<String> {
    val parents = mutableListOf<String>()
    var next: String? = start
    while (true) {
        next = map.get(next)
        if (next == null) { return parents.reversed() }
        parents.add(next)
    }
}

fun countOrbitsToSanta(input: String): Int {
    val map = parse(input)
    val myParents = parentsOf(map, "YOU")
    val santasParents = parentsOf(map, "SAN")
    val commonCount = myParents
            .zip(santasParents)
            .filter { (a, b) -> a == b }
            .count()
    return myParents.size + santasParents.size - (commonCount * 2)
}

fun part2() {
    val count = countOrbitsToSanta(input())
    println("part2: $count")
}


fun all() {
    println("\n** Day 6 **")
    val ms1 = measureTimeMillis { part1() }
    println("---> ${ms1}ms")
    val ms2 = measureTimeMillis { part2() }
    println("---> ${ms2}ms")
}
