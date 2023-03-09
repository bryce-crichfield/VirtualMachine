trait Value
case class Symbol(sym: String) extends Value
case class Number(value: Double) extends Value
case class Chars(value: String) extends Value
// case class Function(name: String, bytecode: Int) extends Value
