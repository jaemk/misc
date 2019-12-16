package d01

import kotlin.test.Test
import kotlin.test.assertEquals


class D01Test {
    @Test fun testMassToFuel() {
        val cases = listOf(
                12 to 2,
                14 to 2,
                1969 to 654,
                100756 to 33583
        )
        for ((inp, exp) in cases) {
            assertEquals(exp, massToFuel(inp))
        }
    }

    @Test fun testMassToFuelWithFuel() {
        val cases = listOf(
                14 to 2,
                1969 to 966,
                100756 to 50346
        )
        for ((inp, exp) in cases) {
            assertEquals(exp, massToFuelWithFuel(inp))
        }
    }
}
