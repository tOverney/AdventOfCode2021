package ch.overney.aoc.day15

import ch.overney.aoc.harness.AppWithInput
import ch.overney.aoc.harness.Point

object Part2 extends AppWithInput("day15", 315L):

  override def solve(dataSet: Iterator[String]): Long =
    val baseRiskLevels: Map[Point, Int] =
      (
        for {
          (row, y) <- dataSet.toSeq.zipWithIndex
          (cell, x) <- row.zipWithIndex
        } yield Point(x, y) -> cell.toString.toInt
      ).toMap

    val baseXSize = baseRiskLevels.keys.map(_.x).max + 1
    val baseYSize = baseRiskLevels.keys.map(_.y).max + 1

    val riskLevels: Map[Point, Int] =
      (
        for {
          xFactor <- 0 to 4
          yFactor <- 0 to 4
          (Point(x, y), risk) <- baseRiskLevels
          newRisk = risk + xFactor + yFactor
          scaledRisk = if newRisk > 9 then newRisk % 9 else newRisk
        } yield Point(x + xFactor * baseXSize, y + yFactor * baseYSize) -> scaledRisk
      ).toMap

    val destination = Point(riskLevels.keys.map(_.x).max, riskLevels.keys.map(_.y).max)

    val startingPoint = Point(0L, 0L)

    def inScope(p: Point): Boolean =
      p.x >= startingPoint.x &&
        p.y >= startingPoint.y &&
        p.x <= destination.x &&
        p.y <= destination.y

    val accessCost =
      collection.mutable.HashMap.from(riskLevels.map { case (p, _) =>
        p -> (if p == startingPoint then 0 else Long.MaxValue)
      })

    implicit object PointOrdering extends Ordering[(Long, Point)] {
      def compare(a: (Long, Point), b: (Long, Point)) =
        val costDiff = a._1.compare(b._1)
        if costDiff == 0 then
          val yDiff = a._2.y.compare(b._2.y)
          if yDiff == 0 then a._2.x.compare(b._2.x)
          else yDiff
        else costDiff
    }

    val unvisited: collection.mutable.SortedSet[(Long, Point)] =
      collection.mutable.SortedSet.from(accessCost.toSeq.map { case (k, v) => (v, k) })(PointOrdering)

    @scala.annotation.tailrec
    def step(
        cost: Long,
        curr: Point
    ): Long =
      val currCost = accessCost(curr)
      curr.orthogonalNeighbors
        .filter(neigh => inScope(neigh) && unvisited((accessCost(neigh), neigh)))
        .foreach { p =>
          val newCost = currCost + riskLevels(p)
          val oldCost = accessCost(p)
          if newCost < oldCost then
            accessCost.update(p, newCost)
            unvisited.remove((oldCost, p))
            unvisited.add((newCost, p))
        }
      require(unvisited.remove((cost, curr)), s"could not remove $curr, $currCost")
      if curr == destination then accessCost(destination)
      else
        val (cst, next) = unvisited.min
        step(cst, next)

    step(0L, startingPoint)
