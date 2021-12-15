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

    type Cont = (Point, Set[Point], Long)

    def step(
        curr: Point,
        seen: Set[Point],
        cost: Long,
        minCostFound: Long = Long.MaxValue
    ): Either[Long, Seq[Cont]] =
      if curr == destination then Left(cost)
      else
        val newCost = cost + riskLevels(curr)
        val posibleNexts = curr.orthogonalNeighbors.filter(neigh => !seen(neigh) && riskLevels.contains(neigh))
        val newContinuations: Seq[Cont] = posibleNexts.map(next => (next, seen + curr, newCost))
        Right(newContinuations)

    @scala.annotation.tailrec
    def nextInLine(minCost: Long, conts: Seq[Cont]): Long =
      val cts: Seq[Cont] = conts.filter(_._3 < minCost)
      if (cts.isEmpty) minCost
      else
        val (c, s, cst) = cts.head
        val (newMin, newConts) =
          step(c, s, cst, minCost).fold(f => (Math.min(f, minCost), Seq()), cs => (minCost, cs))
        nextInLine(newMin, newConts ++ cts.tail)

    nextInLine(Long.MaxValue, Seq((startingPoint, Set(startingPoint), 0L)))
