package ch.overney.aoc.day1

object Part1 extends App:
  val measurements = Input.DataEntries.head.map(_.toLong)
  val res =
    measurements.zip(measurements.tail).foldLeft(0L) { case (acc, (prev, curr)) =>
      if (curr > prev) acc + 1L else acc
    }
  println(res)
