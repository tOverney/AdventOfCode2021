package ch.overney.aoc.day11

import ch.overney.aoc.harness.AppWithInput
import ch.overney.aoc.harness.Point

import scala.annotation.tailrec

object Part2 extends AppWithInput("day11", 195L):

  private lazy val totDaysToRun = 100L
  override def solve(dataSet: Iterator[String]): Long =
    val startingState: Seq[(Point, Int)] =
      for {
        (row, y) <- dataSet.toSeq.zipWithIndex
        (cell, x) <- row.zipWithIndex
      } yield Point(x, y) -> cell.toString.toInt

    val state = collection.mutable.Map.from(startingState)

    @tailrec
    def resolveFlashes(flashes: Long): Long =
      val flashing = state.toSeq.collect { case (point, v) if v > 9 => point }
      if flashing.isEmpty then flashes
      else
        flashing.foreach(state.updateWith(_)(_.map(_ => 0)))
        for {
          cell <- flashing
          neigh <- cell.neighbors()
        } state.updateWith(neigh)(_.map(v => if (v == 0) 0 else v + 1))
        resolveFlashes(flashes + flashing.size)

    @tailrec
    def findAllFlashes(currentDay: Long = 1L): Long =
      state.mapValuesInPlace((_, v) => v + 1)
      val newFlashesSeen = resolveFlashes(0L)
      if newFlashesSeen == state.size then currentDay
      else findAllFlashes(currentDay + 1L)

    findAllFlashes()
