package ch.overney.aoc.day4

import ch.overney.aoc.harness.AppWithInput

object Part2 extends AppWithInput("day4", 1924L):
  override def solve(dataSet: Iterator[String]): Long =
    val (numbers, boards) = BingoBoard.parseAll(dataSet)

    def drawNumber(remainingDraws: Seq[Int], notCompletedBoards: Seq[BingoBoard]): (Int, BingoBoard) =
      assert(remainingDraws.nonEmpty, "No winning boards??")
      val drawnNumber = remainingDraws.head
      notCompletedBoards.foreach(_.markNumber(drawnNumber))
      val (justCompleted, stillNot) = notCompletedBoards.partition(_.isCompleted)
      justCompleted match
        case Seq(board) if stillNot.isEmpty => (drawnNumber, board)
        case _ if stillNot.isEmpty          => sys.error(s">1?? Num: $drawnNumber, boards: $notCompletedBoards")
        case _                              => drawNumber(remainingDraws.tail, stillNot)

    val (lastDraw, winningBoard) = drawNumber(numbers, boards)
    println(s"Last winner found! Number $lastDraw Board:\n$winningBoard")
    val unmarkedSum = winningBoard.lines.flatMap(_.collect { case Cell(num, false) => num.toLong }).sum
    unmarkedSum * lastDraw
