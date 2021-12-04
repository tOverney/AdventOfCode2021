package ch.overney.aoc.day4

import ch.overney.aoc.harness.AppWithInput

object Part1 extends AppWithInput("day4", 4512L):

  override def solve(dataSet: Iterator[String]): Long =
    val (numbers, boards) = BingoBoard.parseAll(dataSet)

    def drawNumber(remainingDraws: Seq[Int]): (Int, BingoBoard) =
      assert(remainingDraws.nonEmpty, "No winning boards??")
      val drawnNumber = remainingDraws.head
      boards.foreach(_.markNumber(drawnNumber))
      boards.find(_.isCompleted) match {
        case Some(board) => (drawnNumber, board)
        case None        => drawNumber(remainingDraws.tail)
      }

    val (lastDraw, winningBoard) = drawNumber(numbers)
    println(s"Winner found! Number $lastDraw Board:\n$winningBoard")
    val unmarkedSum = winningBoard.lines.flatMap(_.collect { case Cell(num, false) => num.toLong }).sum
    unmarkedSum * lastDraw
