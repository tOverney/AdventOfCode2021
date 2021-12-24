package ch.overney.aoc.harness

import scala.collection.immutable.NumericRange

final case class Point3d(x: Long, y: Long, z: Long) extends Ordered[Point3d]:

  def +(other: Point3d): Point3d = Point3d(x + other.x, y + other.y, z + other.z)
  def -(other: Point3d): Point3d = Point3d(x - other.x, y - other.y, z - other.z)

  val coords = Set(() => x, () => y, () => z)

  def possibleTranslations: Seq[Point3d] =
    Seq(
      Point3d(x, y, z),
      Point3d(y, z, x),
      Point3d(z, x, y),
      Point3d(-x, z, y),
      Point3d(z, y, -x),
      Point3d(y, -x, z),
      Point3d(x, z, -y),
      Point3d(z, -y, x),
      Point3d(-y, x, z),
      Point3d(x, -z, y),
      Point3d(-z, y, x),
      Point3d(y, x, -z),
      Point3d(-x, -y, z),
      Point3d(-y, z, -x),
      Point3d(z, -x, -y),
      Point3d(-x, y, -z),
      Point3d(y, -z, -x),
      Point3d(-z, -x, y),
      Point3d(x, -y, -z),
      Point3d(-y, -z, x),
      Point3d(-z, x, -y),
      Point3d(-x, -z, -y),
      Point3d(-z, -y, -x),
      Point3d(-y, -x, -z)
    )

  def compare(other: Point3d): Int =
    val xDiff = x.compare(other.x)
    if xDiff == 0 then
      val yDiff = y.compare(other.y)
      if yDiff == 0 then z.compare(other.z) else yDiff
    else xDiff

  def distanceTo(other: Point3d): Long =
    val diff = this - other
    diff.x.abs + diff.y.abs + diff.z.abs

object Point3d:
  def apply(raw: String): Point3d =
    val s"$x,$y,$z" = raw
    Point3d(x.toLong, y.toLong, z.toLong)
