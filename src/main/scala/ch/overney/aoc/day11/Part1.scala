package ch.overney.aoc.day11

import ch.overney.aoc.harness.AppWithInput
import ch.overney.aoc.harness.Point

import scala.annotation.tailrec

object Part1 extends AppWithInput("day11", 1656L, Seq("tiny.txt" -> 259L)):

  private def printState(state: Map[Point, Int]): Unit =
    val range = 0 until state.count { case (p, _) => p.x == 0 }
    val map =
      range
        .map(y => range.map(x => state(Point(x, y))).map(v => if (v > 9) "X" else v.toString).mkString(""))
        .mkString("\n")
    println(map)

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
      println("---")
      printState(state.toMap)
      println("---")
      if flashing.isEmpty then flashes
      else
        flashing.foreach(state.updateWith(_)(_.map(_ => 0)))
        for {
          cell <- flashing
          neigh <- cell.neighbors
        } state.updateWith(neigh)(_.map(v => if (v == 0) 0 else v + 1))
        resolveFlashes(flashes + flashing.size)

    @tailrec
    def accumulatedDailyFlashes(remainingDays: Long, flashesSeen: Long): Long =
      println(s"after ${totDaysToRun - remainingDays}")
      printState(state.toMap)
      if remainingDays == 0L then flashesSeen
      else
        state.mapValuesInPlace((_, v) => v + 1)
        val newFlashesSeen = resolveFlashes(flashesSeen)
        accumulatedDailyFlashes(remainingDays - 1L, newFlashesSeen)

    val res = accumulatedDailyFlashes(totDaysToRun, 0L)
    res
