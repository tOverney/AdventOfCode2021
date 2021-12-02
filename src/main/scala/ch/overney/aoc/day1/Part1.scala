package ch.overney.aoc.day1

import ch.overney.aoc.harness.AppWithInput

object Part1 extends AppWithInput("day1", sampleAnswer = 7L):

  override def solve(dataSet: Iterator[String]): Long =
    val measurements = dataSet.map(_.toLong).toSeq

    measurements
      .zip(measurements.drop(1))
      .foldLeft(0L) { case (acc, (prev, curr)) =>
        if (curr > prev) acc + 1L else acc
      }
