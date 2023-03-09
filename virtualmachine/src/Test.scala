object Test  {
    val file = "/home/bryce/Desktop/VirtualMachine/virtualmachine/src/test.vm"
    val loader = Loader.load(file)
    loader.parse()
    if (loader.isError) { println(loader.error) }

    val machine = new VirtualMachine(loader.constants(), loader.bytecode())

    machine.execute()

    if (machine.isError) { println(machine.error) }

    Debug[VirtualMachine].dump(machine)
}

object TestLinker {
    val add2 = ProgramUnit(
      Symbol("add2"),
      List(
        Symbol("mock1") -> Number(1)

      ),
      List(
        Dup(),
        Add(),
        Store(Symbol("c1")),
        Add(),
        Load(Symbol("c1")),
        Add(),
        Ret()
      )
    )

    val sub2 = ProgramUnit(
      Symbol("sub2"),
      List(
        Symbol("mock1") -> Number(1)
      ),
      List(
        Dup(),
        Sub(),
        Store(Symbol("c1")),
        Sub(),
        Load(Symbol("c1")),
        Sub(),
        Ret()
      )
    )

    val main = ProgramUnit(
      Symbol("main"),
      List(
        Symbol("f1") -> Function("add2"),
        Symbol("f2") -> Function("sub2"),
        Symbol("c1") -> Number(1),
        Symbol("c2") -> Number(2),
        Symbol("n1") -> Number(2)
      ),
      List(
        Push(Symbol("c1")),
        Push(Symbol("c2")),
        Call(Symbol("f1"), Symbol("n1")),
        Push(Symbol("c1")),
        Call(Symbol("f2"), Symbol("n1")),
        Ret()
      )
    )

    val units = List(add2, sub2, main)
    val relocated = relocate(units)
    val normalized = normalize(relocated)
    val collated = collate(normalized)
    val tabulated = tabulate(collated.bytecode)
    val resolved = resolve(collated)
    val linked = link(resolved)

    def main(args: Array[String]): Unit = {
        println("----------------------------------------------------------------")
        units.foreach(u => Debug[ProgramUnit].dump(u))
        println("----------------------------------------------------------------")
        relocated.foreach(u => Debug[ProgramUnit].dump(u))
        println("----------------------------------------------------------------")
        normalized.foreach(u => Debug[ProgramUnit].dump(u))
        println("----------------------------------------------------------------")
        println(collated)
        println("----------------------------------------------------------------")
        println(tabulated)
        println("----------------------------------------------------------------")
        println(resolved)
        println("----------------------------------------------------------------")
        println(linked)
        println("----------------------------------------------------------------")
    }
}