object Test extends App {
    val file = "/home/bryce/Desktop/VirtualMachine/virtualmachine/src/test.vm"
    val loader = Loader.load(file)
    loader.parse()
    if (loader.isError) { println(loader.error) }

    val machine = new VirtualMachine(loader.constants(), loader.bytecode())

    machine.execute()

    if (machine.isError) { println(machine.error) }

    Debug[VirtualMachine].dump(machine)
}
