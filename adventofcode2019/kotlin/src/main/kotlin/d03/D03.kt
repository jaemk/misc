package d03

import utils.memoize
import utils.readFile
import java.lang.Exception
import java.lang.IllegalStateException
import kotlin.math.absoluteValue
import kotlin.system.measureTimeMillis


enum class Dir {
    UP,
    DOWN,
    LEFT,
    RIGHT;

    companion object {
        @Throws(IllegalStateException::class)
        fun parse(s: Char): Dir {
            return when (s) {
                'U' -> UP
                'D' -> DOWN
                'L' -> LEFT
                'R' -> RIGHT
                else -> {
                    throw IllegalStateException("invalid direction $s")
                }
            }
        }
    }
}

data class Move(val dir: Dir, val count: Int) {
    companion object {
        fun parse(s: String): Move {
            val dir = Dir.parse(s[0])
            val count = s.slice(1 until s.length).toInt()
            return Move(dir, count)
        }
    }
}


data class Point(var x: Int, var y: Int) {
    fun distanceFromZero(): Int {
        return this.x.absoluteValue + this.y.absoluteValue
    }
}

data class PathPoint(var x: Int, var y: Int, var pathDist: Int) {
    fun inc(dir: Dir) {
        this.pathDist++
        when (dir) {
            Dir.UP -> this.y++
            Dir.DOWN -> this.y--
            Dir.LEFT -> this.x--
            Dir.RIGHT -> this.x++
        }
    }

    fun point(): Point {
        return Point(this.x, this.y)
    }
}


fun parseInput(s: String): Pair<List<Move>, List<Move>> {
    val moves = s
            .trim()
            .split("\n")
            .map { it.split(",").map { Move.parse(it) } }
    return moves[0] to moves[1]
}

val input: () -> String = {
    readFile("../input/d03.txt").readText()
}.memoize()


fun pointSeq(moves: List<Move>): Sequence<PathPoint> {
    val point = PathPoint(0, 0, 0)
    return moves.asSequence().flatMap { move ->
        var count = move.count
        generateSequence {
            if (count == 0) {
                null
            } else {
                point.inc(move.dir)
                count--
                point.copy()
            }
        }
    }
}

fun findIntersections(a: List<Move>, b: List<Move>): Sequence<Point> {
    val seen = pointSeq(a).mapTo(mutableSetOf(), { it.point() })
    return pointSeq(b).map { it.point() }.filter { seen.contains(it) }
}

fun findMinIntersectionDistance(a: List<Move>, b: List<Move>): Int {
    val minPoint = findIntersections(a, b).minBy { it.distanceFromZero() }
            ?: throw Exception("no intersections found")
    return minPoint.distanceFromZero()
}


fun findMinIntersectionPath(a: List<Move>, b: List<Move>): Int {
    val seen = mutableMapOf<Point, Int>()
    for (p in pointSeq(a)) {
        val point = p.point()
        val dist = seen.getOrPut(point, { p.pathDist })
        if (p.pathDist < dist) {
            seen[point] = p.pathDist
        }
    }
    return pointSeq(b)
            .map { it to (seen.get(it.point()) ?: 0) }
            .filter { (_, bDist) ->
                bDist != 0
            }
            .map { (a, bDist) -> a.pathDist + bDist }
            .min() ?: throw Exception("no intersections found")
}


fun part1() {
    val (a, b) = parseInput(input())
    val dist = findMinIntersectionDistance(a, b)
    println("part1: $dist")
}

fun part2() {
    val (a, b) = parseInput(input())
    val dist = findMinIntersectionPath(a, b)
    println("part2: $dist")
}

fun all() {
    println("\n** Day 3 **")
    val ms1 = measureTimeMillis { part1() }
    println("---> ${ms1}ms")
    val ms2 = measureTimeMillis { part2() }
    println("---> ${ms2}ms")
}