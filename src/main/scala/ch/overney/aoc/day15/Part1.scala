package ch.overney.aoc.day15

import ch.overney.aoc.harness.AppWithInput
import ch.overney.aoc.harness.Point

object Part1 extends AppWithInput("day15", 40L):

  override def solve(dataSet: Iterator[String]): Long =
    val riskLevels: Map[Point, Int] =
      (
        for {
          (row, y) <- dataSet.toSeq.zipWithIndex
          (cell, x) <- row.zipWithIndex
        } yield Point(x, y) -> cell.toString.toInt
      ).toMap

    val destination = Point(riskLevels.keys.map(_.x).max, riskLevels.keys.map(_.y).max)

    val startingPoint = Point(0L, 0L)
    val accessCost =
      collection.mutable.Map.from(riskLevels.map { case (p, _) =>
        p -> (if p == startingPoint then 0 else Long.MaxValue)
      })

    @scala.annotation.tailrec
    def step(
        curr: Point,
        unvisited: Set[Point]
    ): Long =
      val currCost = accessCost(curr)
      //println(s"visting $curr ($currCost)")
      curr.orthogonalNeighbors
        .filter(neigh => unvisited(neigh) && riskLevels.contains(neigh))
        .foreach { p =>
          val newCost = currCost + riskLevels(p)
          if newCost < accessCost(p) then accessCost.update(p, newCost)
        }
      val newUnvisited = unvisited - curr
      if curr == destination then accessCost(destination)
      else step(newUnvisited.minBy(accessCost(_)), newUnvisited)

    step(startingPoint, riskLevels.keySet)
