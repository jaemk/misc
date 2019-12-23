package d06

import org.junit.Test
import kotlin.test.assertEquals


class D06Test {
    @Test
    fun testPart1() {
        val inp = """
            COM)B
            B)C
            C)D
            D)E
            E)F
            B)G
            G)H
            D)I
            E)J
            J)K
            K)L
        """.trimIndent()
        assertEquals(42, countOrbits(inp))
    }

    @Test
    fun testPart2() {
        val inp = """
            COM)B
            B)C
            C)D
            D)E
            E)F
            B)G
            G)H
            D)I
            E)J
            J)K
            K)L
            K)YOU
            I)SAN
        """.trimIndent()
        assertEquals(4, countOrbitsToSanta(inp))
    }
}
