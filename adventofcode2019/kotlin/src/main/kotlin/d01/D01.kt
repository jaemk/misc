package d01

import utils.readFile
import kotlin.math.floor
import kotlin.system.measureTimeMillis

fun input(): List<Int> {
    val parsed = mutableListOf<Int>()
    readFile("../input/d01.txt")
            .useLines { it.mapTo(parsed, { line -> line.toInt() }) }
    return parsed.toList()
}

fun massToFuel(mass: Int): Int = floor(mass / 3.0).toInt() - 2

fun part1() {
    val res = input().fold(0, { acc, mass -> acc + massToFuel(mass) })
    println("part1: $res")
}


fun massToFuelWithFuel(mass: Int): Int {
    var totalFuel = 0
    var remainingMass = massToFuel(mass)
    while (remainingMass > 0) {
        totalFuel += remainingMass
        remainingMass = massToFuel(remainingMass)
    }
    return totalFuel
}

fun part2() {
    val res = input().fold(0, { acc, mass -> acc + massToFuelWithFuel(mass) })
    println("part2: $res")
}

fun all() {
    println("** Day 1 **")
    val ms1 = measureTimeMillis { part1() }
    println("---> ${ms1}ms")

    val ms2 = measureTimeMillis { part2() }
    println("---> ${ms2}ms")
}
