package d05

import org.junit.Test
import utils.Vm
import kotlin.test.assertEquals

class D05Test {
    @Test
    fun testSampleCode() {
        val res = Vm(listOf(1002, 4, 3, 4, 33)).runToCompletion()
        assertEquals(1002, res)
    }

    @Test
    fun testNegatives() {
        val res = Vm(listOf(1101, 100, -1, 4, 0)).runToCompletion()
        assertEquals(1101, res)
    }

    @Test
    fun testInOut() {
        val readVal = 200
        fun read(): Int {
            return readVal
        }

        val output = mutableListOf<Int>()
        fun write(value: Int) {
            output.add(value)
        }

        val vm = Vm(listOf(3, 0, 4, 0, 99), readFn = ::read, writeFn = ::write)
        val res = vm.runToCompletion()

        assertEquals(200, res)
        assertEquals(listOf(200), output)
    }

    @Test
    fun testEqualsPositional() {
        var readVal: Int? = null
        val output = mutableListOf<Int>()
        fun read(): Int {
            return readVal!!
        }

        fun write(value: Int) {
            output.add(value)
        }

        // read input to index-9 (overwrite -1)
        // cmp 8 == 8, set index-9 to 1
        // output index-9, stop
        readVal = 8
        val vm = Vm(
                listOf(3, 9, 8, 9, 10, 9, 4, 9, 99, -1, 8),
                readFn = ::read,
                writeFn = ::write)
        val res1 = vm.runToCompletion()
        assertEquals(3, res1)
        assertEquals(listOf(1), output)

        // cmp 9 == 8, set index-1 to 0
        readVal = 9
        vm.reset()
        output.clear()
        val res2 = vm.runToCompletion()
        assertEquals(3, res2)
        assertEquals(listOf(0), output)
    }

    @Test
    fun testEqualsImmediate() {
        var readVal: Int? = null
        val output = mutableListOf<Int>()
        fun read(): Int {
            return readVal!!
        }

        fun write(value: Int) {
            output.add(value)
        }

        // cmp 8 == 8
        readVal = 8
        val vm = Vm(
                listOf(3, 3, 1108, -1, 8, 3, 4, 3, 99),
                readFn = ::read,
                writeFn = ::write)
        val res1 = vm.runToCompletion()
        assertEquals(3, res1)
        assertEquals(listOf(1), output)

        // cmp 9 == 8, set index-1 to 0
        readVal = 9
        vm.reset()
        output.clear()
        val res2 = vm.runToCompletion()
        assertEquals(3, res2)
        assertEquals(listOf(0), output)
    }

    @Test
    fun testLessThanPositional() {
        var readVal: Int? = null
        val output = mutableListOf<Int>()
        fun read(): Int {
            return readVal!!
        }

        fun write(value: Int) {
            output.add(value)
        }

        // cmp if input is less-than 8
        readVal = 7
        val vm = Vm(
                listOf(3, 9, 7, 9, 10, 9, 4, 9, 99, -1, 8),
                readFn = ::read,
                writeFn = ::write)
        val res1 = vm.runToCompletion()
        assertEquals(3, res1)
        assertEquals(listOf(1), output)

        readVal = 8
        vm.reset()
        output.clear()
        val res2 = vm.runToCompletion()
        assertEquals(3, res2)
        assertEquals(listOf(0), output)
    }

    @Test
    fun testLessThanImmediate() {
        var readVal: Int? = null
        val output = mutableListOf<Int>()
        fun read(): Int {
            return readVal!!
        }

        fun write(value: Int) {
            output.add(value)
        }

        // cmp if input is less-than 8
        readVal = 7
        val vm = Vm(
                listOf(3, 3, 1107, -1, 8, 3, 4, 3, 99),
                readFn = ::read,
                writeFn = ::write)
        val res1 = vm.runToCompletion()
        assertEquals(3, res1)
        assertEquals(listOf(1), output)

        readVal = 8
        vm.reset()
        output.clear()
        val res2 = vm.runToCompletion()
        assertEquals(3, res2)
        assertEquals(listOf(0), output)
    }

    @Test
    fun testJumpPositional() {
        var readVal: Int? = null
        val output = mutableListOf<Int>()
        fun read(): Int {
            return readVal!!
        }

        fun write(value: Int) {
            output.add(value)
        }

        // output zero if input is zero, else 1
        readVal = 0
        val vm = Vm(
                listOf(3, 12, 6, 12, 15, 1, 13, 14, 13, 4, 13, 99, -1, 0, 1, 9),
                readFn = ::read,
                writeFn = ::write)
        val res1 = vm.runToCompletion()
        assertEquals(3, res1)
        assertEquals(listOf(0), output)

        readVal = 1
        vm.reset()
        output.clear()
        val res2 = vm.runToCompletion()
        assertEquals(3, res2)
        assertEquals(listOf(1), output)
    }

    @Test
    fun testJumpImmediate() {
        var readVal: Int? = null
        val output = mutableListOf<Int>()
        fun read(): Int {
            return readVal!!
        }

        fun write(value: Int) {
            output.add(value)
        }

        // output zero if input is zero, else 1
        readVal = 0
        val vm = Vm(
                listOf(3, 3, 1105, -1, 9, 1101, 0, 0, 12, 4, 12, 99, 1),
                readFn = ::read,
                writeFn = ::write)
        val res1 = vm.runToCompletion()
        assertEquals(3, res1)
        assertEquals(listOf(0), output)

        readVal = 1
        vm.reset()
        output.clear()
        val res2 = vm.runToCompletion()
        assertEquals(3, res2)
        assertEquals(listOf(1), output)
    }

    @Test
    fun testLargerCodeSample() {
        var readVal: Int? = null
        val output = mutableListOf<Int>()
        fun read(): Int {
            return readVal!!
        }

        fun write(value: Int) {
            output.add(value)
        }

        // output 999 if input < 8, 1000 if input == 8, 1001 if input > 8
        readVal = 7
        val vm = Vm(
                listOf(3, 21, 1008, 21, 8, 20, 1005, 20, 22, 107, 8, 21, 20, 1006, 20, 31,
                        1106, 0, 36, 98, 0, 0, 1002, 21, 125, 20, 4, 20, 1105, 1, 46, 104,
                        999, 1105, 1, 46, 1101, 1000, 1, 20, 4, 20, 1105, 1, 46, 98, 99),
                readFn = ::read,
                writeFn = ::write)
        val res1 = vm.runToCompletion()
        assertEquals(3, res1)
        assertEquals(listOf(999), output)

        readVal = 8
        vm.reset()
        output.clear()
        val res2 = vm.runToCompletion()
        assertEquals(3, res2)
        assertEquals(listOf(1000), output)

        readVal = 9
        vm.reset()
        output.clear()
        val res3 = vm.runToCompletion()
        assertEquals(3, res2)
        assertEquals(listOf(1001), output)
    }
}
