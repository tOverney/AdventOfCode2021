package ch.overney.aoc.day4

final case class BingoBoard(lines: IndexedSeq[IndexedSeq[Cell]]):
  import BingoBoard._

  def markNumber(numDrawn: Int): Unit =
    for {
      line <- lines
      cell <- line
      if cell.num == numDrawn
    } cell.marked = true

  def isCompleted: Boolean =
    lines.exists(_.forall(_.marked)) ||
      (0 until BoardSize).exists(idx => lines.forall(_(idx).marked))

  override def toString: String =
    lines
      .map(_.mkString(" "))
      .mkString("\n")

object BingoBoard:
  val BoardSize = 5

  def parseAll(dataSet: Iterator[String]): (Seq[Int], Seq[BingoBoard]) =
    val input = dataSet.toIndexedSeq.filter(_.nonEmpty)
    val numberDraw = input.head.split(",").map(_.toInt)
    val boards =
      input.tail
        .map(_.split(" ").toIndexedSeq.collect { case n if n.nonEmpty => n.toInt })
        .sliding(BoardSize, BoardSize)
        .toSeq
        .map(rows => BingoBoard(rows.map(_.toIndexedSeq.map(Cell(_)))))

    (numberDraw, boards)
