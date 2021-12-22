package ch.overney.aoc.day22

import ch.overney.aoc.harness.Point3d
import scala.collection.immutable.NumericRange

final case class Cube(isOn: Boolean, minX: Long, maxX: Long, minY: Long, maxY: Long, minZ: Long, maxZ: Long):
  val xRange = minX to maxX
  val yRange = minY to maxY
  val zRange = minZ to maxZ
  val isOff = !isOn

  def allPos: Seq[Point3d] =
    for {
      x <- xRange
      y <- yRange
      z <- zRange
    } yield Point3d(x, y, z)

  lazy val size: Long = (Math.abs(maxX - minX) + 1) * (Math.abs(maxY - minY) + 1) * (Math.abs(maxZ - minZ) + 1)

  def intersect(other: Cube): Option[Cube] =
    val intersectXRange = xRange.intersect(other.xRange)
    val intersectYRange = yRange.intersect(other.yRange)
    val intersectZRange = zRange.intersect(other.zRange)
    Option.when(
      intersectXRange.nonEmpty &&
        intersectYRange.nonEmpty &&
        intersectZRange.nonEmpty
    )(
      Cube(
        isOn = false,
        intersectXRange.min,
        intersectXRange.max,
        intersectYRange.min,
        intersectYRange.max,
        intersectZRange.min,
        intersectZRange.max
      )
    )

  def diff(toRemove: Cube): Seq[Cube] =
    (
      for {
        (mminX, mmaxX) <- Seq((minX, toRemove.minX - 1), (toRemove.minX, toRemove.maxX), (toRemove.maxX + 1, maxX))
        if mmaxX - mminX >= 0L
        (mminY, mmaxY) <- Seq((minY, toRemove.minY - 1), (toRemove.minY, toRemove.maxY), (toRemove.maxY + 1, maxY))
        if mmaxY - mminY >= 0L
        (mminZ, mmaxZ) <- Seq((minZ, toRemove.minZ - 1), (toRemove.minZ, toRemove.maxZ), (toRemove.maxZ + 1, maxZ))
        if mmaxZ - mminZ >= 0L
        c = Cube(isOn, mminX, mmaxX, mminY, mmaxY, mminZ, mmaxZ)
        if c.copy(isOn = toRemove.isOn) != toRemove
      } yield c
    ).toSeq

  def isInRange(range: Set[Long]): Boolean =
    range.contains(minX) && range.contains(maxX) &&
      range.contains(minY) && range.contains(maxY) &&
      range.contains(minZ) && range.contains(maxZ)

object Cube:
  def apply(input: String): Cube =
    val s"$onOff x=$minX..$maxX,y=$minY..$maxY,z=$minZ..$maxZ" = input
    Cube(onOff == "on", minX.toLong, maxX.toLong, minY.toLong, maxY.toLong, minZ.toLong, maxZ.toLong)

  def unapply(input: String): Option[Cube] = Some(Cube(input))
