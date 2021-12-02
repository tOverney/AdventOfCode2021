package ch.overney.aoc.day1

import ch.overney.aoc.harness.AppWithInput

object Part2 extends AppWithInput("day1", sampleAnswer = 5L):
  override def solve(dataSet: Iterator[String]): Long =
    val measurements = dataSet.map(_.toLong).sliding(3).toSeq
    measurements
      .zip(measurements.drop(1))
      .foldLeft(0L) { case (acc, (prev, curr)) =>
        if (curr.sum > prev.sum) acc + 1L else acc
      }
