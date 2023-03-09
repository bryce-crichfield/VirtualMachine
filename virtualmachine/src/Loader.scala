import scala.io.Source


case class ConstantDefintion(symbol: Symbol, value: Value)

class Loader {
    var isError: Boolean = false
    var error = ""

    private var input: List[String] = List()
    private val consts = SymbolTableBuilder()
    private val instrs = BytecodeBuilder()

    def except(message: String): Unit = {
        error = message
        isError = true
    }

    def load(input: String): Unit = {
        val trimmed = input.trim()
        val lines = trimmed.split("\n")
            .filter(l => l != "\n" && l != "" && l != "\r" && l != "#")
            .map(_.trim().replaceAll("\\s+", " ")).toList
        this.input = lines
    }

    def consume(): Unit = { input = input.tail }

    def parse_constant(): Unit = {
        val line = input.head
        val const = Parser[ConstantDefintion].parse(line) match
            case None => except(s"Invalid constant definition: ${line}")
            case Some(value) =>
                consts.insert(value.symbol, value.value)
                consume()
    }

    def parse_bytecode(): Unit = {
        val line = input.head
        val instr = Parser[Instruction].parse(line) match
            case None => except(s"Invalid instruction: ${line}")
            case Some(value) =>
                instrs.insert(value)
                consume()
    }

    def parse_section_constant(): Unit = {
        input.head match {
            case ".begin_constants" => consume()
            case _ => except(s"Expected .begin_constants, got ${input.head}")
        }

        while (input.head != ".end_constants") {
            parse_constant()
            if (isError) { return }
        }

        consume()
    }

    def parse_section_bytecode(): Unit = {
        input.head match {
            case ".begin_bytecode" => consume()
            case _ => except(s"Expected .begin_bytecode, got ${input.head}")
        }

        while (input.head != ".end_bytecode") {
            parse_bytecode()
            if (isError) { return }
        }

        consume()
    }

    def parse(): Unit = {
        parse_section_constant()
        if (isError) { return }

        parse_section_bytecode()
        if (isError) { return }
    }

    def constants(): SymbolTable = { return consts.build() }

    def bytecode(): Bytecode = { return instrs.build() }
}

object Loader {
    def load(path: String): Loader = {
        val loader = new Loader()
        val source = Source.fromFile(path)
        val input = source.getLines().mkString("\n")
        source.close()

        loader.load(input)
        loader
    }
}
