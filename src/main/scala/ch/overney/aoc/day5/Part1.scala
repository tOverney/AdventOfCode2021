package ch.overney.aoc.day5

import ch.overney.aoc.harness.AppWithInput
import ch.overney.aoc.harness.Point

object Part1 extends AppWithInput("day5", 5):
  override def solve(dataSet: Iterator[String]): Int =
    val lines = Point.allLines(dataSet)

    Point.situationMap(lines).values.count(_ > 1)
