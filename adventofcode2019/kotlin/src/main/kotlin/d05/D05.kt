package d05

import utils.Vm
import utils.memoize
import utils.readFile
import kotlin.system.measureTimeMillis

val input = {
    readFile("../input/d05.txt").readText()
}.memoize()

fun parseInput(s: String): List<Int> {
    return s.trim().split(",").map { it.toInt() }
}

fun part1() {
    val code = parseInput(input())
    val read = { 1 }

    val buffer = mutableListOf<Int>()
    fun write(value: Int) { buffer.add(value) }
    val vm = Vm(code, readFn = read, writeFn = ::write)
    vm.runToCompletion()

    val end = buffer.last()
    val ok = buffer.size == 10 && end == 15386262
    println("part1: $end, $ok")
}


fun part2() {
    val res = 1
    println("part2: $res")
}


fun all() {
    println("\n** Day 5 **")
    val ms1 = measureTimeMillis { part1() }
    println("---> ${ms1}ms")
    val ms2 = measureTimeMillis { part2() }
    println("---> ${ms2}ms")
}
