package ch.overney.aoc.day10

import ch.overney.aoc.harness.AppWithInput
import ch.overney.aoc.harness.Point
import scala.collection.mutable.Stack

object Part2 extends AppWithInput("day10", 288957L):
  private def closingCost(input: String): Long =
    val expectedClose = Stack[Char]()
    @scala.annotation.tailrec
    def iter(remainder: Seq[Char]): Long =
      remainder.headOption match
        case None =>
          expectedClose.popAll.reverse.foldLeft(0L)((acc, char) => acc * 5 + Chunk.byClosing(char).closingCost)
        case Chunk(chunk) =>
          expectedClose.push(chunk.closing)
          iter(remainder.tail)
        case Some(closingChar) =>
          val chunk = Chunk.values.find(_.closing == closingChar).get
          if expectedClose.pop == closingChar then iter(remainder.tail)
          else 0L

    iter(input)

  def median(s: Seq[Long]): Long =
    val (lower, upper) = s.sortWith(_ < _).splitAt(s.size / 2)
    if (s.size % 2 == 0) (lower.last + upper.head) / 2L else upper.head

  override def solve(dataSet: Iterator[String]): Long =
    median(dataSet.toSeq.map(closingCost).filterNot(_ == 0L))
