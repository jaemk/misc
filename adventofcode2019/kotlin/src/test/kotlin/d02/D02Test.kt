package d02

import org.junit.Test
import kotlin.test.assertEquals

class D02Test {
    @Test
    fun test_rolling_window() {
        val windows = rollingWindow(5)
                .map { it.toList() }
                .take(4)
                .toList()

        assertEquals(
                listOf(
                        listOf(0, 1, 2, 3),
                        listOf(4, 0, 1, 2),
                        listOf(3, 4, 0, 1),
                        listOf(2, 3, 4, 0)
                ),
                windows
        )
    }

    @Test
    fun test_code() {
        val code = listOf(1, 9, 10, 3, 2, 3, 11, 0, 99, 30, 40, 50)
        val res = runCode(code)
        assertEquals(3500, res)
    }
}
