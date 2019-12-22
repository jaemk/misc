package d05

import org.junit.Test
import utils.Vm
import kotlin.test.assertEquals

class D05Test {
    @Test
    fun testSampleCode() {
        val res = Vm(listOf(1002,4,3,4,33)).runToCompletion()
        assertEquals(1002, res)
    }

    @Test
    fun testNegatives() {
        val res = Vm(listOf(1101,100,-1,4,0)).runToCompletion()
        assertEquals(1101, res)
    }

    @Test
    fun testInOut() {
        val readVal = 200
        fun read(): Int { return readVal }

        val output = mutableListOf<Int>()
        fun write(value: Int) { output.add(value) }

        val vm = Vm(listOf(3,0,4,0,99), readFn = ::read, writeFn = ::write)
        val res = vm.runToCompletion()

        assertEquals(200, res)
        assertEquals(listOf(200), output)
    }
}
