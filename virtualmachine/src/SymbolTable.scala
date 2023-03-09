// RUNTIME REPRESENTATION OF SYMBOL TABLES
class SymbolTable(size: Int) {
    private var capacity: Int = 0
    private var table: Map[Symbol, Value] = Map()

    def get(symbol: Symbol): Attempt[Value] = {
        if (!table.contains(symbol)) { return Attempt.failure("Symbol Not Found") }

        return Attempt.success(table(symbol))
    }

    def set(symbol: Symbol, value: Value): Attempt[Unit] = {
        if (!table.contains(symbol)) { capacity += 1 }

        table = table.updated(symbol, value)
        return Attempt.success(())
    }

    def remove(symbol: Symbol): Attempt[Unit] = {
        if (table.contains(symbol)) {
            capacity -= 1
            table = table - symbol
        }

        return Attempt.success(())
    }

    def copy(here: Symbol, there: Symbol): Attempt[Unit] = {
        if (!table.contains(here)) { return Attempt.success(()) }

        if (!table.contains(there)) { capacity += 1 }

        table = table.updated(there, table(here))
        return Attempt.success(())
    }

    def isEmpty(): Boolean = { return capacity == 0 }

    def isFull(): Boolean = { return capacity == size }

    def load(other: SymbolTable): Unit = { table = table ++ other.table }

    def data(): Map[Symbol, Value] = { return table }

    def has(symbol: Symbol): Boolean = { return table.contains(symbol) }
}

class SymbolTableBuilder {
    private var table: Map[Symbol, Value] = Map()

    def insert(symbol: Symbol, value: Value): SymbolTableBuilder = {
        table = table.updated(symbol, value)
        return this
    }

    def has(symbol: Symbol): Boolean = { return table.contains(symbol) }

    def build(): SymbolTable = {
        val symbols = new SymbolTable(table.size)
        for ((symbol, value) <- table) { symbols.set(symbol, value) }
        return symbols
    }
}
