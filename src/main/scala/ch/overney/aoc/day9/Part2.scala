package ch.overney.aoc.day9

import ch.overney.aoc.harness.AppWithInput
import ch.overney.aoc.harness.Point

object Part2 extends AppWithInput("day9", 1134L):
  override def solve(dataSet: Iterator[String]): Long =
    val terrain: IndexedSeq[IndexedSeq[Long]] =
      dataSet.toIndexedSeq.map(_.map(_.toString.toLong).toIndexedSeq)

    val ySize = terrain.size
    val xSize = terrain.head.size

    val bassins = collection.mutable.ListBuffer.empty[Set[Point]]

    def computeBassin(curr: Point, acc: Set[Point] = Set.empty): Set[Point] =
      val currX = curr.x.toInt
      val currY = curr.y.toInt
      val newAcc = acc + curr
      def validateNeighbor(x: Int, y: Int): Option[Point] =
        Option.unless(
          acc.contains(Point(x, y)) || y == -1 || y == ySize || x == -1 || x == xSize || terrain(y)(x) == 9
        )(Point(x, y))

      // horrible recursion
      val topBassin = validateNeighbor(currX, currY - 1).fold(newAcc)(computeBassin(_, newAcc))
      val bottomBassin = validateNeighbor(currX, currY + 1).fold(topBassin)(computeBassin(_, topBassin))
      val leftBassin = validateNeighbor(currX - 1, currY).fold(bottomBassin)(computeBassin(_, bottomBassin))
      validateNeighbor(currX + 1, currY).fold(leftBassin)(computeBassin(_, leftBassin))

    def notAlreadyInBassins(point: Point): Boolean = bassins.forall(!_(point))

    for {
      y <- 0 until ySize
      x <- 0 until xSize
      elem = terrain(y)(x)
      if elem != 9
      point = Point(x, y)
    } if notAlreadyInBassins(point) then
      val bassin = computeBassin(point)
      bassins.addOne(bassin)

    bassins.sortBy(bassin => -bassin.size).take(3).foldLeft(1L)((acc, pt) => acc * pt.size)
