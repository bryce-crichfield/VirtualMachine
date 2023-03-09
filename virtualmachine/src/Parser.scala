trait Parser[I] {
    def parse(input: String): Option[I]
}
object Parser {
    def apply[I](using parser: Parser[I]): Parser[I] = parser

    given Parser[ConstantDefintion] = (input: String) => {
        input.split(" ").toList match {
            case symbol :: "number" :: value :: Nil =>
                Some(ConstantDefintion(Symbol(symbol), Number(value.toDouble)))
            case symbol :: "string" :: value :: Nil =>
                Some(ConstantDefintion(Symbol(symbol), Chars(value)))
            case symbol :: "symbol" :: value :: Nil =>
                Some(ConstantDefintion(Symbol(symbol), Symbol(value)))
            case symbol :: "instr" :: value :: Nil =>
                Some(ConstantDefintion(Symbol(symbol), Number(value.toDouble)))
            case _ => None
        }
    }

    given Parser[Instruction] = (input: String) => {
        input.split(" ").toList match {
            case "halt" :: _  => Parser[Halt].parse(input)
            case "const" :: _ => Parser[Const].parse(input)
            case "load" :: _  => Parser[Load].parse(input)
            case "store" :: _ => Parser[Store].parse(input)
            case "push" :: _  => Parser[Push].parse(input)
            case "pop" :: _   => Parser[Pop].parse(input)
            case "swap" :: _  => Parser[Swap].parse(input)
            case "dup" :: _   => Parser[Dup].parse(input)
            case "add" :: _   => Parser[Add].parse(input)
            case "jump" :: _  => Parser[Jump].parse(input)
            case "call" :: _  => Parser[Call].parse(input)
            case "ret" :: _   => Parser[Ret].parse(input)
            case _            => None
        }
    }

    given Parser[Halt] = (input: String) => {
        input.split(" ").toList match {
            case "halt" :: Nil => Some(Halt())
            case _             => None
        }
    }

    given Parser[Const] = (input: String) => {
        input.split(" ").toList match {
            case "const" :: location :: value :: Nil =>
                Some(Const(Symbol(location), Symbol(value)))
            case _ => None
        }
    }

    given Parser[Load] = (input: String) => {
        input.split(" ").toList match {
            case "load" :: location :: Nil => Some(Load(Symbol(location)))
            case _                         => None
        }
    }

    given Parser[Store] = (input: String) => {
        input.split(" ").toList match {
            case "store" :: location :: Nil => Some(Store(Symbol(location)))
            case _                          => None
        }
    }

    given Parser[Push] = (input: String) => {
        input.split(" ").toList match {
            case "push" :: value :: Nil => Some(Push(Symbol(value)))
            case _                      => None
        }
    }

    given Parser[Pop] = (input: String) => {
        input.split(" ").toList match {
            case "pop" :: Nil => Some(Pop())
            case _            => None
        }
    }

    given Parser[Swap] = (input: String) => {
        input.split(" ").toList match {
            case "swap" :: Nil => Some(Swap())
            case _             => None
        }
    }

    given Parser[Add] = (input: String) => {
        input.split(" ").toList match {
            case "add" :: Nil => Some(Add())
            case _            => None
        }
    }

    given Parser[Jump] = (input: String) => {
        input.split(" ").toList match {
            case "jump" :: value :: Nil => Some(Jump(Symbol(value)))
            case _                      => None
        }
    }

    given Parser[Dup] = (input: String) => {
        input.split(" ").toList match {
            case "dup" :: Nil => Some(Dup())
            case _            => None
        }
    }

    given Parser[Call] = (input: String) => {
        input.split(" ").toList match {
            case "call" :: value :: args :: Nil =>
                Some(Call(Symbol(value), Symbol(args)))
            case _ => None
        }
    }

    given Parser[Ret] = (input: String) => {
        input.split(" ").toList match {
            case "ret" :: Nil => Some(Ret())
            case _            => None
        }
    }
}
