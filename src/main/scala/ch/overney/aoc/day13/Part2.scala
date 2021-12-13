package ch.overney.aoc.day13

import ch.overney.aoc.harness.AppWithInput
import ch.overney.aoc.harness.Point

import scala.annotation.tailrec

object Part2 extends AppWithInput("day13", 16L):

  def display(points: Seq[Point]): Unit =
    val maxX = points.map(_.x).max
    val maxY = points.map(_.y).max

    val res =
      (0L to maxY)
        .map(y => (0L to maxX).map(x => if points.contains(Point(x, y)) then "#" else ".").mkString(""))
        .mkString("\n")
    println(res)

  override def solve(dataSet: Iterator[String]): Long =
    val (pointsRaw, foldsRaw) = dataSet.toSeq.filterNot(_.isEmpty).partition(_.contains(","))
    val folds = foldsRaw.map(FoldInstruction.apply)
    val points = pointsRaw.map(Point.apply)
    val folded = folds.foldLeft(points)((ps, instruction) => instruction.fold(ps))

    display(folded)

    folded.size
