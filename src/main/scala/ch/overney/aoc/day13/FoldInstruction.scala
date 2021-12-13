package ch.overney.aoc.day13

import ch.overney.aoc.day13.Dimension.*
import ch.overney.aoc.harness.Point

final case class FoldInstruction(dimension: Dimension, pos: Long):
  val isInX = dimension == X

  def foldedDim(p: Point) = if isInX then p.x else p.y

  def fold(points: Seq[Point]): Seq[Point] =
    val (toBeFolded, unchanged) =
      points.partition(foldedDim(_) > pos)
    val folded =
      toBeFolded.map { p =>
        val newDimPos = pos - (foldedDim(p) - pos)
        if isInX then p.copy(x = newDimPos) else p.copy(y = newDimPos)
      }
    (folded ++ unchanged).distinct

object FoldInstruction:

  def apply(raw: String): FoldInstruction =
    val s"fold along $dim=$pos" = raw

    FoldInstruction(if (dim == "x") X else Y, pos.toLong)
