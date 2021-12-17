package ch.overney.aoc.day17

import ch.overney.aoc.harness.AppWithInput
import ch.overney.aoc.harness.Point

object Part1 extends AppWithInput("day17", 45L):

  override def solve(dataSet: Iterator[String]): Long =
    val startingPos = Point(0L, 0L)
    val target = Target(dataSet.next)
    println(target)

    def step(pos: Point, veloc: Point, maxHeight: Long): Option[Long] =
      val newPos = pos + veloc
      val newXVeloc =
        if veloc.x == 0 then 0
        else if veloc.x > 0 then veloc.x - 1L
        else veloc.x + 1L
      val newVeloc = Point(newXVeloc, veloc.y - 1L)
      val newMaxHeight = Math.max(pos.y, maxHeight)

      if target.isOn(newPos) then Some(newMaxHeight)
      else if target.isPast(newPos) then None
      else step(newPos, newVeloc, newMaxHeight)

    (
      for {
        x <- 1L to target.minX
        y <- -2L to 100L
      } yield step(startingPos, Point(x, y), 0L)
    ).flatten.max
