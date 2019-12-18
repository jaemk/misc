package d03


import org.junit.Test
import kotlin.test.assertEquals

class D03Test {
    @Test
    fun testPointSeq() {
        val moves = listOf(
                Move(Dir.RIGHT, 2),
                Move(Dir.UP, 2),
                Move(Dir.LEFT, 4),
                Move(Dir.DOWN, 4),
                Move(Dir.RIGHT, 2),
                Move(Dir.UP, 2)
        )
        val points = pointSeq(moves).map { it.point() }.toList()

        assertEquals(
                listOf(
                        Point(1, 0),
                        Point(2, 0),
                        Point(2, 1),
                        Point(2, 2),
                        Point(1, 2),
                        Point(0, 2),
                        Point(-1, 2),
                        Point(-2, 2),
                        Point(-2, 1),
                        Point(-2, 0),
                        Point(-2, -1),
                        Point(-2, -2),
                        Point(-1, -2),
                        Point(0,-2 ),
                        Point(0,-1 ),
                        Point(0,0 )
                ),
                points
        )
    }

    @Test
    fun testPart1() {
        listOf(
                """
                R75,D30,R83,U83,L12,D49,R71,U7,L72
                U62,R66,U55,R34,D71,R55,D58,R83
                """.trimIndent() to 159,
                """
                R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51
                U98,R91,D20,R16,D67,R40,U7,R15,U6,R7
                """.trimIndent() to 135
        ).map { (arg, expected) ->
            val (a, b) = parseInput(arg)
            val d = findMinIntersectionDistance(a, b)
            assertEquals(d, expected)
        }
    }

    @Test
    fun testPart2() {
        listOf(
                """
                R75,D30,R83,U83,L12,D49,R71,U7,L72
                U62,R66,U55,R34,D71,R55,D58,R83
                """.trimIndent() to 610,
                """
                R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51
                U98,R91,D20,R16,D67,R40,U7,R15,U6,R7
                """.trimIndent() to 410
        ).map { (arg, expected) ->
            val (a, b) = parseInput(arg)
            val d = findMinIntersectionPath(a, b)
            assertEquals(d, expected)
        }
    }
}
