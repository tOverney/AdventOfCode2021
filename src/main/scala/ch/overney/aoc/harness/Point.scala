package ch.overney.aoc.harness

import scala.collection.immutable.NumericRange

final case class Point(x: Long, y: Long):
  import Point._

  def to(other: Point, handleDiagonal: Boolean): Seq[Point] =
    other match
      case Point(`x`, `y`)    => Seq(this)
      case Point(`x`, otherY) => minToMax(y, otherY).map(Point(x, _))
      case Point(otherX, `y`) => minToMax(x, otherX).map(Point(_, y))
      case Point(otherX, otherY) if handleDiagonal =>
        minToMax(x, otherX).zip(minToMax(y, otherY)).map { case (x, y) => Point(x, y) }
      case diagonal => Seq()

object Point:
  private def minToMax(a: Long, b: Long): Seq[Long] =
    val range = Math.min(a, b) to Math.max(a, b)
    if (b > a) range.reverse else range

  def allLines(dataSet: Iterator[String]): Seq[(Point, Point)] =
    dataSet.toSeq.collect { case s"$sx,$sy -> $ex,$ey" =>
      (Point(sx.toLong, sy.toLong), Point(ex.toLong, ey.toLong))
    }

  def situationMap(lines: Seq[(Point, Point)], handleDiagonal: Boolean = false): Map[Point, Int] =
    val situationMap = collection.mutable.Map.empty[Point, Int]
    for {
      (start, end) <- lines
      point <- start.to(end, handleDiagonal)
    } situationMap.updateWith(point)(_.fold(Some(1))(c => Some(c + 1)))
    situationMap.toMap
