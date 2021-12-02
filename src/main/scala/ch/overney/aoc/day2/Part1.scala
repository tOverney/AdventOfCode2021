package ch.overney.aoc.day2

object Part1 extends App:
  val res = Input.DataEntries.head.foldLeft((0L, 0L)) { case ((h, v), instr) =>
    val List(direction, rawJump) = instr.split(" ").toList

    val jump = rawJump.toLong
    direction match {
      case "forward" => (h + jump, v)
      case "down"    => (h, v + jump)
      case "up"      => (h, v - jump)
      case _         => sys.error("wut")
    }
  }

  println(s"$res " + (res._1 * res._2))
