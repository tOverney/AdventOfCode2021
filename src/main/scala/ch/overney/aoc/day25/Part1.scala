package ch.overney.aoc.day25

import ch.overney.aoc.harness.AppWithInput
import ch.overney.aoc.harness.Point

import scala.annotation.tailrec

object Part1 extends AppWithInput("day25", 58L):

  override def solve(dataSet: Iterator[String]): Long =
    val input = dataSet.toSeq
    val ySize = input.size
    val xSize = input.head.size

    def display(counter: Long, input: Map[Point, SeaCucumber]): Unit =

      val res =
        (0 until ySize)
          .map(y =>
            (0 until xSize)
              .map { x =>
                input.get(Point(x, y)).fold('.')(_.char)
              }
              .mkString("")
          )
          .mkString("\n")
      println(s"-- $counter --\n$res")

    val state = collection.mutable.Map.from(SeaCucumber.parseAll(input))

    @tailrec
    def step(counter: Long, stateSnapshot: Map[Point, SeaCucumber]): Long =
      display(counter - 1, stateSnapshot)
      val (eastBounds, southBounds) = state.toSeq.sortBy(_._1).partition(_._2.isEastBound)

      def moveAll(cucumbers: Seq[(Point, SeaCucumber)], prevState: Map[Point, SeaCucumber]): Unit =
        for
          (pos, cucumber) <- cucumbers
          newPos = if cucumber.isEastBound then pos.right(xSize) else pos.down(ySize)
          if !prevState.contains(newPos)
        do
          require(state.remove(pos).isDefined)
          state.addOne(newPos -> cucumber)

      moveAll(eastBounds, stateSnapshot)
      val afterEastMovedSnapshot = state.toMap
      moveAll(southBounds, afterEastMovedSnapshot)

      val newSnapshot = state.toMap
      if stateSnapshot == newSnapshot then counter
      else step(counter + 1L, newSnapshot)

    step(1L, state.toMap)
