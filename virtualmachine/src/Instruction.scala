trait Instruction {
    def execute(machine: VirtualMachine): Attempt[Unit]
}

case class Halt() extends Instruction {
    def execute(machine: VirtualMachine): Attempt[Unit] = {
        machine.isRunning = false
        return Attempt.success(())
    }
}

case class Const(location: Symbol, value: Symbol) extends Instruction {
    def execute(machine: VirtualMachine): Attempt[Unit] = {
        for {
            context <- machine.stack.peek()
            const <- machine.constants.get(value)
            _ <- context.locals.set(location, const)
        } yield ()
    }
}

case class Load(location: Symbol) extends Instruction {
    def execute(machine: VirtualMachine): Attempt[Unit] = {
        for {
            context <- machine.stack.peek()
            value <- context.locals.get(location)
            _ <- context.stack.push(value)
        } yield ()
    }
}

case class Store(location: Symbol) extends Instruction {
    def execute(machine: VirtualMachine): Attempt[Unit] = {
        for {
            context <- machine.stack.peek()
            value <- context.stack.pop()
            _ <- context.locals.set(location, value)
        } yield ()
    }
}

case class Push(value: Symbol) extends Instruction {
    def execute(machine: VirtualMachine): Attempt[Unit] = {
        for {
            context <- machine.stack.peek()
            const <- machine.constants.get(value)
            _ <- context.stack.push(const)
        } yield ()
    }
}

case class Pop() extends Instruction {
    def execute(machine: VirtualMachine): Attempt[Unit] = {
        for {
            context <- machine.stack.peek()
            _ <- context.stack.pop()
        } yield ()
    }
}

case class Swap() extends Instruction {
    def execute(machine: VirtualMachine): Attempt[Unit] = {
        for {
            context <- machine.stack.peek()
            a <- context.stack.pop()
            b <- context.stack.pop()
            _ <- context.stack.push(a)
            _ <- context.stack.push(b)
        } yield ()
    }
}

case class Dup() extends Instruction {
    def execute(machine: VirtualMachine): Attempt[Unit] = {
        for {
            context <- machine.stack.peek()
            a <- context.stack.pop()
            _ <- context.stack.push(a)
            _ <- context.stack.push(a)
        } yield ()
    }
}

case class Add() extends Instruction {
    def execute(machine: VirtualMachine): Attempt[Unit] = {
        for {
            context <- machine.stack.peek()
            a <- context.stack.pop()
            b <- context.stack.pop()
            result <- (a, b) match {
                case (Number(a), Number(b)) => Attempt.success(Number(a + b))
                case _ => Attempt.failure(s"Invalid Types ${a} ${b}")
            }
            _ <- context.stack.push(result)
        } yield ()
    }
}

case class Jump(bytecode: Symbol) extends Instruction {
    def execute(machine: VirtualMachine): Attempt[Unit] = {
        for {
            index <- machine.constants.get(bytecode)
            int <- index match {
                case Number(value) => Attempt.success(value.toInt)
                case _             => Attempt.failure(s"Invalid Type ${index}")
            }
            _ <- machine.bytecode.jump(int)
        } yield ()

    }
}

case class Call(function: Symbol, args: Symbol) extends Instruction {
    def execute(machine: VirtualMachine): Attempt[Unit] = {
        // 0. Push return address onto the local stack
        // 1. Push a new local frame onto the global stack
        // 2. Jump to the function in the new local frame
        for {
            context <- machine.stack.peek()
            address <- machine.bytecode.pointer()
            countAttempt <- machine.constants.get(args)
            count <- countAttempt match {
                case Number(value) => Attempt.success(value.toInt)
                case _ => Attempt.failure(s"Invalid Type ${countAttempt}")
            }
            carryover <- context.stack.popmany(count)
            _ <- context.stack.push(Number(address))
            _ <- machine.stack.push(new LocalContext())
            context <- machine.stack.peek()
            _ <- context.stack.pushmany(carryover)
            targetAttempt <- machine.constants.get(function)
            _ <- targetAttempt match {
                case Number(value) => machine.bytecode.jump(value.toInt)
                case _ => Attempt.failure(s"Invalid Target Type ${targetAttempt}")
            }
        } yield ()
    }
}

case class Ret() extends Instruction {
    def execute(machine: VirtualMachine): Attempt[Unit] = {
        // 0. Pop the top value into the return register
        // 1. Pop the local frame off the global stack
        // 2. Pop the return address off the local stack and jump to it
        // 3. Push the return value onto the local stack
        for {
            context <- machine.stack.peek()
            value <- context.stack.pop()
            _ <- machine.stack.pop()
            context <- machine.stack.peek()
            address <- context.stack.pop()
            int <- address match {
                case Number(value) => Attempt.success(value.toInt)
                case _ => Attempt.failure(s"Invalid Address Type ${address}")
            }
            _ <- machine.bytecode.jump(int)
            _ <- context.stack.push(value)
        } yield ()
    }
}
