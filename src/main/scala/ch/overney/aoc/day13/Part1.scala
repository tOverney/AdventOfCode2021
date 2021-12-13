package ch.overney.aoc.day13

import ch.overney.aoc.harness.AppWithInput
import ch.overney.aoc.harness.Point

import scala.annotation.tailrec

object Part1 extends AppWithInput("day13", 17L):

  override def solve(dataSet: Iterator[String]): Long =
    val (pointsRaw, foldsRaw) = dataSet.toSeq.filterNot(_.isEmpty).partition(_.contains(","))
    val folds = foldsRaw.map(FoldInstruction.apply)
    val points = pointsRaw.map(Point.apply)
    folds.head.fold(points).size
