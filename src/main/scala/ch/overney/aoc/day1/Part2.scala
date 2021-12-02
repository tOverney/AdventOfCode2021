package ch.overney.aoc.day1

object Part2 extends App:
  val measurements = Input.DataEntries.head.map(_.toLong)
  val res =
    measurements.sliding(3).zip(measurements.tail.sliding(3)).foldLeft(0L) { case (acc, (prev, curr)) =>
      if (curr.sum > prev.sum) acc + 1L else acc
    }
  println(res)
