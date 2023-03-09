// Created by the loader after parsing a unit
case class ProgramUnit(
    name: Symbol,
    symbols: List[(Symbol, Value)],
    bytecode: List[Instruction]
) {
    def rename(generator: () => String): ProgramUnit = {
        def rename(instruction: Instruction): Instruction = {
            instruction
        }

        val newname = name
        val newsymbols = symbols.map { case (symbol, value) =>
            (Symbol(generator()), value)
        }

        val newbytecode = bytecode.map(rename)

        return ProgramUnit(newname, newsymbols, newbytecode)
    }
}




// Linker Operation:
// 1. Relocate -
//      move units into correct positions
// 2. Normalize -
//      rename symbols to be unique globally
// 3. Collation -
//      move symbols into a global symbol table
//      move bytecode into a global bytecode table
// 3. Tabulate -
//      calculate bytecode offsets for each unit
// 4. Resolve -
//      resolve all symbols in the global symbol table to a terminal
//      reduce all symbols in the global symbol table to a single symbol
//      rename any bytecode references to the old symbol to the new symbol
// 5. Link -
//      create a single bytecode and symbol table

// Used by the linker, useful type alias
case class LinkingTable(
    symboltable: Map[Symbol, Value],
    bytecode: Map[Symbol, List[Instruction]]
)

// TODO: Add error handling
def relocate(units: List[ProgramUnit]): List[ProgramUnit] = {
    // Get the main unit, place it at the front of the list
    // Position all other units behind the main unit

    val (main, others) = units.partition(_.name == Symbol("main"))
    return main ++ others
}

def normalize(units: List[ProgramUnit]): List[ProgramUnit] = {
    var count = 0
    var counter = () => { count += 1; s"gen_$count" }
    return units.map { unit => unit.rename(counter) }
}

def collate(units: List[ProgramUnit]): LinkingTable = {
    return LinkingTable(Map(), Map())
}
def tabulate(bytecode: Map[Symbol, List[Instruction]]): Map[Symbol, Int] = {
    return Map()
}

def resolve(table: LinkingTable): LinkingTable = { return table }

def link(table: LinkingTable): (SymbolTable, Bytecode) = {
    val symbols = SymbolTableBuilder()
    for ((symbol, value) <- table.symboltable) { symbols.insert(symbol, value) }

    val bytecode = BytecodeBuilder()
    for ((symbol, instructions) <- table.bytecode) {
        for (instruction <- instructions) { bytecode.insert(instruction) }
    }

    return (symbols.build(), bytecode.build())
}
