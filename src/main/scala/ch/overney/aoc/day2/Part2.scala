package ch.overney.aoc.day2

object Part2 extends App:
  val res = Input.DataEntries.head.foldLeft((0L, 0L, 0L)) { case ((h, v, a), instr) =>
    val List(direction, rawJump) = instr.split(" ").toList

    val jump = rawJump.toLong
    direction match {
      case "forward" => (h + jump, v + (jump * a), a)
      case "down"    => (h, v, a + jump)
      case "up"      => (h, v, a - jump)
      case _         => sys.error("wut")
    }
  }

  println(s"$res " + (res._1 * res._2))
