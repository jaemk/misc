package utils


enum class Mode {
    POS,  // value must be read from memory
    IMM;  // value is the literal value

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
    HLT,  // halt(0) ---------- stop execution
    ADD,  // add(3) ----------- add arg1 and arg2, store in arg3
    MUL,  // multiply(3) ------ multiple arg1 and arg2, store in arg3
    JIT,  // jump-if-true(2) -- if arg1 != 0, store arg2 at the instr ptr
    JIF,  // jump-if-false(2) - if arg1 == 0, store arg2 at the instr ptr
    ILT,  // is-less-than(3) -- if arg1 < arg2, store 1 at arg3
    IEQ,  // is-equal(3) ------ if arg1 == arg2, store 1 at arg3
    INP,  // read-input(1) ---- read from "input" and store at arg1
    OUT;  // write-output(2) -- write arg1 to "output"

    companion object {
        fun parse(s: String): OpCode {
            return when (s) {
                "01" -> ADD
                "02" -> MUL
                "03" -> INP
                "04" -> OUT
                "05" -> JIT
                "06" -> JIF
                "07" -> ILT
                "08" -> IEQ
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
            Mode.IMM -> throw IllegalStateException("writing to an immediate position is not supported")
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
            val adv = when (op.code) {
                OpCode.HLT -> return this.mem[0]
                OpCode.ADD -> {
                    val numArgs = 3
                    val (a, b, out) = this.loadArgs(numArgs, op.modes)
                    val res = a.read() + b.read()
                    out.write(res)
                    numArgs + 1
                }
                OpCode.MUL -> {
                    val numArgs = 3
                    val (a, b, out) = this.loadArgs(numArgs, op.modes)
                    val res = a.read() * b.read()
                    out.write(res)
                    numArgs + 1
                }
                OpCode.JIT -> {
                    val numArgs = 2
                    val (a, b) = this.loadArgs(numArgs, op.modes)
                    val advance = if (a.read() != 0) {
                        this.ptr = b.read()
                        0
                    } else {
                        numArgs + 1
                    }
                    advance
                }
                OpCode.JIF -> {
                    val numArgs = 2
                    val (a, b) = this.loadArgs(numArgs, op.modes)
                    val advance = if (a.read() == 0) {
                        this.ptr = b.read()
                        0
                    } else {
                        numArgs + 1
                    }
                    advance
                }
                OpCode.ILT -> {
                    val numArgs = 3
                    val (a, b, out) = this.loadArgs(numArgs, op.modes)
                    if (a.read() < b.read()) {
                        out.write(1)
                    } else {
                        out.write(0)
                    }
                    numArgs + 1
                }
                OpCode.IEQ -> {
                    val numArgs = 3
                    val (a, b, out) = this.loadArgs(numArgs, op.modes)
                    if (a.read() == b.read()) {
                        out.write(1)
                    } else {
                        out.write(0)
                    }
                    numArgs + 1
                }
                OpCode.INP -> {
                    val numArgs = 1
                    val (out) = this.loadArgs(numArgs, op.modes)
                    val res = this.read()
                    out.write(res)
                    numArgs + 1
                }
                OpCode.OUT -> {
                    val numArgs = 1
                    val (value) = this.loadArgs(numArgs, op.modes)
                    this.write(value.read())
                    numArgs + 1
                }
            }
            this.ptr += adv
        }
    }
}