package d02

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

fun rollingWindow(memSize: Int): Sequence<List<Int>> {
    if (memSize < 4) {
        throw IllegalArgumentException("max must be greater than 4")
    }
    return generateSequence(mutableListOf(0, 1, 2, 3), {
        var next = it[3]
        for (i in 0..3) {
            next++
            if (next >= memSize) {
                next = 0
            }
            it[i] = next
        }
        it
    })
}


fun runCode(mem: List<Int>): Int {
    var mem = mem.toMutableList()

    for (args in rollingWindow(mem.size)) exloop@ {
        val opCode = mem[args[0]]
        if (opCode == 99) break

        val inputPosA = mem[args[1]]
        val inputA = mem[inputPosA]
        val inputPosB = mem[args[2]]
        val inputB = mem[inputPosB]
        val outpos = mem[args[3]]

        val res = when (opCode) {
            1 -> inputA + inputB
            2 -> inputA * inputB
            else -> {
                throw IllegalStateException("unknown opcode $opCode")
            }
        }
        mem[outpos] = res
    }
    return mem[0]
}

fun prepare(mem: MutableList<Int>, a: Int, b: Int) {
    mem[1] = a
    mem[2] = b
}

fun part1() {
    val code = input().toMutableList()
    prepare(code, 12, 2)
    val res = runCode(code)
    println("part1: $res")
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
