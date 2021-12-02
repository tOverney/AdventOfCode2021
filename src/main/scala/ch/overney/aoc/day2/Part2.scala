package ch.overney.aoc.day2

import ch.overney.aoc.harness.AppWithInput

object Part2 extends AppWithInput("day2", 900L):

  override def solve(dataSet: Iterator[String]): Long =
    val (horizontal, depth, _) =
      dataSet.foldLeft((0L, 0L, 0L)) { case ((h, v, a), instr) =>
        val List(direction, rawJump) = instr.split(" ").toList

        val jump = rawJump.toLong
        direction match {
          case "forward" => (h + jump, v + (jump * a), a)
          case "down"    => (h, v, a + jump)
          case "up"      => (h, v, a - jump)
          case _         => sys.error("wut")
        }
      }

    horizontal * depth
