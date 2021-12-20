package ch.overney.aoc.day20

import ch.overney.aoc.day16.binToLong
import ch.overney.aoc.harness.AppWithInput
import ch.overney.aoc.harness.Point

object Part2 extends AppWithInput("day20", 3351L):

  private def normalizeChars(input: String): Seq[Char] =
    input.map(c => if c == '#' then '1' else '0')

  override def solve(dataSet: Iterator[String]): Long =
    val ref = normalizeChars(dataSet.next).toArray
    val terrain =
      dataSet.toArray
        .filterNot(_.isEmpty)
        .zipWithIndex
        .flatMap { case (row, y) =>
          normalizeChars(row).zipWithIndex.collect { case (cell, x) => Point(x, y) -> cell }
        }
        .toMap

    def enhance(input: Map[Point, Char], default: Char, rem: Int): Map[Point, Char] =
      if rem == 0 then input
      else
        val points = input.keySet
        val xs = points.map(_.x)
        val minX = xs.min
        val maxX = xs.max
        val ys = points.map(_.y)
        val minY = ys.min
        val maxY = ys.max
        val newInput =
          (
            for {
              y <- (minY - 1) to (maxY + 1)
              x <- (minX - 1) to (maxX + 1)
              curr = Point(x, y)
              neighs = curr.neighbors(includeSelf = true).map(input.getOrElse(_, default)).mkString
            } yield curr -> ref(binToLong(neighs).toInt)
          ).toMap
        enhance(newInput, if rem % 2 == 0 || ref.head == '0' then ref.head else ref.last, rem - 1)

    val enhanced = enhance(terrain, '0', rem = 50)
    enhanced.count(_._2 == '1')
