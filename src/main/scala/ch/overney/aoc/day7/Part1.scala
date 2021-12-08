package ch.overney.aoc.day7

import ch.overney.aoc.harness.AppWithInput
import ch.overney.aoc.harness.Point

object Part1 extends AppWithInput("day7", 37L):
  override def solve(dataSet: Iterator[String]): Long =
    val crabs = dataSet.next.split(",").map(_.toInt)
    val min = crabs.min
    val max = crabs.max

    def computeFuelCost(target: Int): Long =
      crabs.foldLeft(0L)((acc, crab) => acc + Math.abs(target - crab))

    (min to max).foldLeft(Long.MaxValue)((best, targetPos) => Math.min(best, computeFuelCost(targetPos)))
