package ch.overney.aoc.day2

import ch.overney.aoc.harness.AppWithInput

object Part1 extends AppWithInput("day2", 150L):

  override def solve(dataSet: Iterator[String]): Long =
    val (horizontal, depth) =
      dataSet.foldLeft((0L, 0L)) { case ((h, v), instr) =>
        val List(direction, rawJump) = instr.split(" ").toList

        val jump = rawJump.toLong
        direction match
          case "forward" => (h + jump, v)
          case "down"    => (h, v + jump)
          case "up"      => (h, v - jump)
          case _         => sys.error("wut")
      }

    horizontal * depth
