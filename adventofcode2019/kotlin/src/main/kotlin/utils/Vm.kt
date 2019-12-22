package utils


enum class Mode {
    POS,
    IMM;

    companion object {
        fun parse(c: Char): Mode {
            return when (c) {
                '0' -> POS
                '1' -> IMM
                else -> {
                    throw IllegalStateException("invalid mode $c")
                }
            }
        }
    }
}

enum class OpCode {
    HLT,
    ADD,
    MUL,
    INP,
    OUT;

    companion object {
        fun parse(s: String): OpCode {
            return when (s) {
                "01" -> ADD
                "02" -> MUL
                "03" -> INP
                "04" -> OUT
                "99" -> HLT
                else -> {
                    throw IllegalStateException("invalid opcode $s")
                }
            }
        }
    }
}

class Op(val code: OpCode, val modes: Triple<Mode, Mode, Mode>) {
    companion object {
        fun parse(opCode: Int): Op {
            val s = opCode.toString().padStart(5, '0')
            val mode3 = Mode.parse(s[0])
            val mode2 = Mode.parse(s[1])
            val mode1 = Mode.parse(s[2])
            val code = OpCode.parse(s.substring(3))
            return Op(code, Triple(mode1, mode2, mode3))
        }
    }
}

class Access(private val mem: MutableList<Int>, private val value: Int, private val mode: Mode) {
    fun read(): Int {
        return when (mode) {
            Mode.POS -> this.mem[value]
            Mode.IMM -> value
        }
    }

    fun write(value: Int) {
        when (mode) {
            Mode.IMM -> throw IllegalStateException("writing to an immediate position makes no sense")
            Mode.POS -> this.mem[this.value] = value
        }
    }
}

class Vm(private val code: List<Int>,
         private val readFn: (() -> Int)? = null,
         private val writeFn: ((Int) -> Unit)? = null) {
    var mem: MutableList<Int> = code.toMutableList()
    var ptr: Int = 0

    fun reset() {
        this.mem = code.toMutableList()
        this.ptr = 0
    }

    private fun defaultRead(): Int {
        print("(vm)=> ")
        return readLine()?.toInt() ?: throw IllegalArgumentException("unable to read input")
    }

    private fun defaultWrite(value: Int) {
        println("(vm)>>> $value")
    }

    private fun read(): Int {
        val reader = this.readFn ?: this::defaultRead
        return reader()
    }

    private fun write(value: Int) {
        val writer = this.writeFn ?: this::defaultWrite
        writer(value)
    }

    fun loadArgs(numArgs: Int, modes: Triple<Mode, Mode, Mode>): List<Access> {
        if (numArgs > 3) {
            throw IllegalStateException("max accepted args is 3, requested $numArgs")
        }
        val argPtr = this.ptr + 1
        val args = this.mem.subList(argPtr, argPtr + numArgs)
        return args.zip(modes.toList()).map { (value, mode) ->
            Access(this.mem, value, mode)
        }
    }

    fun runToCompletion(): Int {
        while (true) {
            val op = Op.parse(this.mem[ptr])
            var numArgs = 0
            when (op.code) {
                OpCode.HLT -> return this.mem[0]
                OpCode.ADD -> {
                    numArgs = 3
                    val (a, b, out) = this.loadArgs(numArgs, op.modes)
                    val res = a.read() + b.read()
                    out.write(res)
                }
                OpCode.MUL -> {
                    numArgs = 3
                    val (a, b, out) = this.loadArgs(numArgs, op.modes)
                    val res = a.read() * b.read()
                    out.write(res)
                }
                OpCode.INP -> {
                    numArgs = 1
                    val (out) = this.loadArgs(numArgs, op.modes)
                    val res = this.read()
                    out.write(res)
                }
                OpCode.OUT -> {
                    numArgs = 1
                    val (value) = this.loadArgs(numArgs, op.modes)
                    this.write(value.read())
                }
            }
            val advance = numArgs + 1
            this.ptr += advance
        }
    }
}