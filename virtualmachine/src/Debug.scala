trait Debug[A] {
    def dump(value: A, name: String = ""): Unit
}

object Debug {
    def apply[A](using d: Debug[A]): Debug[A] = d

    given Debug[SymbolTable] = (self: SymbolTable, name: String) => {
        println(f"SymbolTable ${name} {")
        self.data()
            .foreach((symbol, value) => { println(s"  ${symbol.sym} = ${value}") })
        println("}\n")
    }

    given Debug[Bytecode] = (self: Bytecode, name: String) => {
        println(f"Bytecode ${name} {")
        self.data().foreach { instr => println(s"  ${instr}") }
        println("}\n")
    }

    given [A]: Debug[Stack[A]] = (self: Stack[A], name: String) => {
        println(f"Stack ${name} {")
        self.data().foreach { value => println(s"  ${value}") }
        println("}\n")
    }

    given Debug[VirtualMachine] = (self: VirtualMachine, name: String) => {
        Debug[Bytecode].dump(self.bytecode, "Bytecode")
        Debug[SymbolTable].dump(self.constants, "Constants")
        Debug[SymbolTable].dump(self.globals, "Globals")

        self.stack.peek().map { context =>
            Debug[SymbolTable].dump(context.locals, "Locals")
            Debug[Stack[Value]].dump(context.stack, "Stack")
        }()
    }
}
