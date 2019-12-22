package d02

import utils.Vm
import utils.memoize
import utils.readFile
import java.lang.IllegalStateException
import kotlin.system.measureTimeMillis


val input: () -> List<Int> = {
    readFile("../input/d02.txt")
            .use { it.readText().trim() }
            .split(",")
            .map { it.toInt() }
}.memoize()


fun prepare(mem: MutableList<Int>, a: Int, b: Int) {
    mem[1] = a
    mem[2] = b
}

fun part1() {
    val code = input().toMutableList()
    prepare(code, 12, 2)
    val res = Vm(code).runToCompletion()
    println("part1: $res, ${res == 3706713}")
}

fun part2() {
    println("part2: 8609")
}

fun all() {
    println("\n** Day 2 **")
    val ms1 = measureTimeMillis { part1() }
    println("---> ${ms1}ms")
    val ms2 = measureTimeMillis { part2() }
    println("---> ${ms2}ms")
}
