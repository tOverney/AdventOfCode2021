package ch.overney.aoc.day24

enum Value:
  case Var(id: String) extends Value
  case Lit(value: Long) extends Value

object Value:
  def value(v: Value, state: collection.mutable.Map[Var, Long]): Long =
    v match
      case va: Var  => state.getOrElse(va, 0L)
      case lit: Lit => lit.value

  def apply(input: String): Value =
    input match {
      case s if s.toLongOption.isDefined => Lit(s.toLong)
      case _                             => Var(input)
    }

  def varOnly(input: String): Var =
    apply(input) match
      case v: Var => v
      case _: Lit => sys.error("unwanted")
