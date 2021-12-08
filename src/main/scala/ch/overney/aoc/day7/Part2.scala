package ch.overney.aoc.day7

import ch.overney.aoc.harness.AppWithInput
import ch.overney.aoc.harness.Point

object Part2 extends AppWithInput("day7", 168L):
  override def solve(dataSet: Iterator[String]): Long =
    val crabs = dataSet.next.split(",").map(_.toInt)
    val min = crabs.min
    val max = crabs.max

    def sumOfFirstNInteger(n: Int): Int = n * (n + 1) / 2

    def computeFuelCost(target: Int): Long =
      crabs.foldLeft(0L)((acc, crab) => acc + sumOfFirstNInteger(Math.abs(target - crab)))

    (min to max).foldLeft(Long.MaxValue)((best, targetPos) => Math.min(best, computeFuelCost(targetPos)))
