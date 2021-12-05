package ch.overney.aoc.day5

import ch.overney.aoc.harness.AppWithInput
import ch.overney.aoc.harness.Point

object Part2 extends AppWithInput("day5", 12):

  override def solve(dataSet: Iterator[String]): Int =
    val lines = Point.allLines(dataSet)

    Point.situationMap(lines, handleDiagonal = true).values.count(_ > 1)
