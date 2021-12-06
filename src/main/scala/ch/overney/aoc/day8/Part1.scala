package ch.overney.aoc.day8

import ch.overney.aoc.harness.AppWithInput
import ch.overney.aoc.harness.Point

object Part1 extends AppWithInput("day8", 26L):
  private lazy val UniquePatternLength = Set(2, 3, 4, 7)
  override def solve(dataSet: Iterator[String]): Long =
    dataSet.foldLeft(0L) { case (acc, s"$input | $output") =>
      acc + output.split(" ").count(pat => UniquePatternLength(pat.size))
    }
