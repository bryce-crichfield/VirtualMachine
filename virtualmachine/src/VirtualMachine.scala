class LocalContext {
    val stack = new Stack[Value](10)
    val locals = new SymbolTable(10)
}

class VirtualMachine(
    val constants: SymbolTable = new SymbolTable(10),
    val bytecode: Bytecode = new Bytecode()
) {
    var isRunning = true
    var isError = false

    var error = ""

    val stack = new Stack[LocalContext](10)
    val globals = new SymbolTable(10)

    def except(message: String): Unit = {
        error = message
        isError = true
        isRunning = false
    }

    def execute(): Unit = {
        stack.push(new LocalContext())
        while (isRunning) {
            bytecode.next() match {
                case Some(instruction) => instruction.execute(this)() match
                        case Success(_)       => ()
                        case Failure(message) => except(s"$instruction: $message")
                case None => except("Bytecode Exhausted")
            }
        }
    }
}
