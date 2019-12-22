package d02

import org.junit.Test
import utils.Vm
import kotlin.test.assertEquals

class D02Test {
    @Test
    fun test_code() {
        val code = listOf(1, 9, 10, 3, 2, 3, 11, 0, 99, 30, 40, 50)
        val res = Vm(code).runToCompletion()
        assertEquals(3500, res)
    }
}
