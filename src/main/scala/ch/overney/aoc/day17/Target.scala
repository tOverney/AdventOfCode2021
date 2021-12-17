package ch.overney.aoc.day17

import scala.collection.immutable.NumericRange.Inclusive
import ch.overney.aoc.harness.Point

final case class Target(minX: Long, maxX: Long, minY: Long, maxY: Long):
  val xRange: Inclusive[Long] = minX to maxX
  val yRange: Inclusive[Long] = minY to maxY

  def isOn(p: Point): Boolean =
    xRange.contains(p.x) && yRange.contains(p.y)

  def isPast(p: Point): Boolean =
    p.x > maxX || p.y < minY

object Target:
  def apply(raw: String): Target =
    val s"target area: x=$minX..$maxX, y=$minY..$maxY" = raw
    Target(minX.toLong, maxX.toLong, minY.toLong, maxY.toLong)
