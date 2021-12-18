package ch.overney.aoc.day18

import ch.overney.aoc.harness.AppWithInput
import ch.overney.aoc.harness.Point

object Part2 extends AppWithInput("day18", 3993L):

  override def solve(dataSet: Iterator[String]): Long =
    val inputs = dataSet.toSeq.map(Token.ize(_))
    val pairs = inputs.map(Pair(_)).collect { case p: Pair => p }
    println(pairs.mkString("\n"))
    val magnitudes =
      for {
        a <- pairs
        b <- pairs
        if a != b
      } yield (a + b).reduce().magnitude
    magnitudes.max
