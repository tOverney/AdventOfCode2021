package ch.overney.aoc.day24

import ch.overney.aoc.day24.Value.*

enum Instruction(val execute: (collection.mutable.Map[Var, Long], Iterator[Long]) => Unit):

  case Input(a: Var) extends Instruction((state, input) => state.update(a, input.next))
  case Add(a: Var, b: Value) extends Instruction((s, _) => s.update(a, value(a, s) + value(b, s)))
  case Mul(a: Var, b: Value) extends Instruction((s, _) => s.update(a, value(a, s) * value(b, s)))
  case Div(a: Var, b: Value) extends Instruction((s, _) => s.update(a, value(a, s) / value(b, s)))
  case Mod(a: Var, b: Value) extends Instruction((s, _) => s.update(a, value(a, s) % value(b, s)))
  case Eql(a: Var, b: Value) extends Instruction((s, _) => s.update(a, if value(a, s) == value(b, s) then 1 else 0))

object Instruction:

  def apply(input: String): Instruction =
    input match
      case s"inp $v" => Input(Value.varOnly(v))
      case s"$binOp $v1 $v2" =>
        val a = Value.varOnly(v1)
        val b = Value(v2)
        binOp match
          case "add" => Add(a, b)
          case "mul" => Mul(a, b)
          case "div" => Div(a, b)
          case "mod" => Mod(a, b)
          case "eql" => Eql(a, b)
