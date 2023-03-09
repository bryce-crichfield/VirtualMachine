class Bytecode() {
    private var codes: List[Instruction] = List()
    private var counter: Int = 0

    def next(): Option[Instruction] = {
        if (counter < 0 || counter >= codes.length) { return None }

        val code = codes(counter)
        counter += 1
        return Some(code)
    }

    def pointer(): Attempt[Int] = { return Attempt.success(counter) }

    def jump(index: Int): Attempt[Unit] = {
        counter = index
        return Attempt.success(())
    }

    def load(instructions: List[Instruction]): Unit = { codes = instructions }

    def data(): List[Instruction] = { return codes }
}

class BytecodeBuilder() {
    private var codes: List[Instruction] = List()

    def insert(instruction: Instruction): BytecodeBuilder = {
        codes = instruction :: codes
        return this
    }

    def build(): Bytecode = {
        val bytecode = new Bytecode()
        bytecode.load(codes.reverse)
        return bytecode
    }
}
