package ch.overney.aoc.harness

import scala.collection.immutable.NumericRange

final case class Point3d(x: Long, y: Long, z: Long):

  def +(other: Point3d): Point3d = Point3d(x + other.x, y + other.y, z + other.z)

object Point3d:
  def apply(raw: String): Point3d =
    val s"$x,$y,$z" = raw
    Point3d(x.toLong, y.toLong, z.toLong)
