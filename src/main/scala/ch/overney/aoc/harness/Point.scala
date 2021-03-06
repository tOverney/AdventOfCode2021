package ch.overney.aoc.harness

import scala.collection.immutable.NumericRange

final case class Point(x: Long, y: Long) extends Ordered[Point]:
  import Point._

  def +(other: Point): Point = Point(x + other.x, y + other.y)

  def right(untilBound: Long): Point =
    if x + 1 >= untilBound then Point(0L, y) else Point(x + 1L, y)

  def down(untilBound: Long): Point =
    if y + 1 >= untilBound then Point(x, 0L) else Point(x, y + 1L)

  def orthogonalNeighbors: Seq[Point] =
    for {
      (xDelta, yDelta) <- OrthogonalNeighborsDelta
    } yield Point(xDelta + x, yDelta + y)

  def neighbors(includeSelf: Boolean = false): Seq[Point] =
    for {
      yDelta <- NeighborsDelta
      xDelta <- NeighborsDelta
      if includeSelf || xDelta != 0 || yDelta != 0
    } yield Point(xDelta + x, yDelta + y)

  def to(other: Point, handleDiagonal: Boolean): Seq[Point] =
    other match
      case Point(`x`, `y`)    => Seq(this)
      case Point(`x`, otherY) => minToMax(y, otherY).map(Point(x, _))
      case Point(otherX, `y`) => minToMax(x, otherX).map(Point(_, y))
      case Point(otherX, otherY) if handleDiagonal =>
        minToMax(x, otherX).zip(minToMax(y, otherY)).map { case (x, y) => Point(x, y) }
      case diagonal => Seq()

  def compare(other: Point): Int =
    val yDiff = y.compare(other.y)
    if yDiff == 0 then x.compare(other.x) else yDiff

object Point:
  private val NeighborsDelta = Seq(-1L, 0L, 1L)
  private val OrthogonalNeighborsDelta = Seq((-1, 0), (0, -1), (1, 0), (0, 1))
  private def minToMax(a: Long, b: Long): Seq[Long] =
    a to b by (if (b < a) -1 else 1)

  def allLines(dataSet: Iterator[String]): Seq[(Point, Point)] =
    dataSet.toSeq.collect { case s"$sx,$sy -> $ex,$ey" =>
      (Point(sx.toLong, sy.toLong), Point(ex.toLong, ey.toLong))
    }

  def apply(raw: String): Point =
    val s"$x,$y" = raw
    Point(x.toLong, y.toLong)

  def situationMap(lines: Seq[(Point, Point)], handleDiagonal: Boolean = false): Map[Point, Int] =
    val situationMap = collection.mutable.Map.empty[Point, Int]
    for {
      (start, end) <- lines
      point <- start.to(end, handleDiagonal)
    } situationMap.updateWith(point)(_.fold(Some(1))(c => Some(c + 1)))
    situationMap.toMap
