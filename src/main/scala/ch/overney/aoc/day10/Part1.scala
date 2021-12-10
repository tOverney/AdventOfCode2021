package ch.overney.aoc.day10

import ch.overney.aoc.harness.AppWithInput
import ch.overney.aoc.harness.Point

import scala.collection.mutable.Stack

object Part1 extends AppWithInput("day10", 26397L):

  private def corruptionLevel(input: String): Long =
    val expectedClose = Stack[Char]()
    @scala.annotation.tailrec
    def iter(remainder: Seq[Char]): Long =
      remainder.headOption match
        case None => 0L
        case Chunk(chunk) =>
          expectedClose.push(chunk.closing)
          iter(remainder.tail)
        case Some(closingChar) =>
          val chunk = Chunk.byClosing(closingChar)
          if expectedClose.pop == closingChar then iter(remainder.tail)
          else chunk.corruptionCost

    iter(input)

  override def solve(dataSet: Iterator[String]): Long =
    dataSet.foldLeft(0L)((acc, line) => acc + corruptionLevel(line))
