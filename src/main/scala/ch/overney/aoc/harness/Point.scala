package ch.overney.aoc.harness

import scala.collection.immutable.NumericRange

final case class Point(x: Long, y: Long):
  import Point._

  def neighbors: Seq[Point] =
    for {
      xDelta <- NeighborsDelta
      yDelta <- NeighborsDelta
      if xDelta != 0 || yDelta != 0
    } yield Point(xDelta + x, yDelta + y)

  def to(other: Point, handleDiagonal: Boolean): Seq[Point] =
    other match
      case Point(`x`, `y`)    => Seq(this)
      case Point(`x`, otherY) => minToMax(y, otherY).map(Point(x, _))
      case Point(otherX, `y`) => minToMax(x, otherX).map(Point(_, y))
      case Point(otherX, otherY) if handleDiagonal =>
        minToMax(x, otherX).zip(minToMax(y, otherY)).map { case (x, y) => Point(x, y) }
      case diagonal => Seq()

object Point:
  private val NeighborsDelta = Seq(-1L, 0L, 1L)
  private def minToMax(a: Long, b: Long): Seq[Long] =
    a to b by (if (b < a) -1 else 1)

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
