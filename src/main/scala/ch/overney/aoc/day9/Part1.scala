package ch.overney.aoc.day9

import ch.overney.aoc.harness.AppWithInput
import ch.overney.aoc.harness.Point

object Part1 extends AppWithInput("day9", 15L):
  override def solve(dataSet: Iterator[String]): Long =
    val terrain: IndexedSeq[IndexedSeq[Int]] =
      dataSet.toIndexedSeq.map(_.map(_.toString.toInt).toIndexedSeq)

    val ySize = terrain.size
    val xSize = terrain.head.size

    def isLowerThanAllNeighbors(elem: Int, y: Int, x: Int): Boolean =
      val topNeighbor = y == 0 || terrain(y - 1)(x) > elem
      val bottomNeighbor = y + 1 == ySize || terrain(y + 1)(x) > elem
      val leftNeighbor = x == 0 || terrain(y)(x - 1) > elem
      val rightNeighbor = x + 1 == xSize || terrain(y)(x + 1) > elem

      topNeighbor && bottomNeighbor && leftNeighbor && rightNeighbor

    val lowPoints =
      for {
        y <- 0 until ySize
        x <- 0 until xSize
        elem = terrain(y)(x)
        if isLowerThanAllNeighbors(elem, y, x)
      } yield elem.toLong

    lowPoints.sum + lowPoints.size
