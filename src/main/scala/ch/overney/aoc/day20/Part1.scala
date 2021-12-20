package ch.overney.aoc.day20

import ch.overney.aoc.day16.binToLong
import ch.overney.aoc.harness.AppWithInput
import ch.overney.aoc.harness.Point

object Part1 extends AppWithInput("day20", 35L):

  def display(input: Map[Point, Char]): Unit =
    val points = input.filter(_._2 == '1').keySet
    val xs = points.map(_.x)
    val minX = xs.min
    val maxX = xs.max
    val ys = points.map(_.y)
    val minY = ys.min
    val maxY = ys.max

    val res =
      (minY to maxY)
        .map(y => (minX to maxX).map(x => if points.contains(Point(x, y)) then "#" else ".").mkString(""))
        .mkString("\n")
    println(s"---\n$res")

  private def normalizeChars(input: String): Seq[Char] =
    input.map(c => if c == '#' then '1' else '0')

  override def solve(dataSet: Iterator[String]): Long =
    val ref = normalizeChars(dataSet.next)
    val terrain =
      dataSet.toArray
        .filterNot(_.isEmpty)
        .zipWithIndex
        .flatMap { case (row, y) =>
          normalizeChars(row).zipWithIndex.collect { case (cell, x) => Point(x, y) -> cell }
        }
        .toMap
    display(terrain)

    def enhance(input: Map[Point, Char], default: Char): Map[Point, Char] =
      val points = input.keySet
      val xs = points.map(_.x)
      val minX = xs.min
      val maxX = xs.max
      val ys = points.map(_.y)
      val minY = ys.min
      val maxY = ys.max
      (
        for {
          y <- (minY - 1) to (maxY + 1)
          x <- (minX - 1) to (maxX + 1)
          curr = Point(x, y)
          neighs = curr.neighbors(includeSelf = true).map(input.getOrElse(_, default)).mkString
        } yield curr -> ref(binToLong(neighs).toInt)
      ).toMap

    val enhancedOnce = enhance(terrain, '0')
    display(enhancedOnce)
    val enhancedTwice = enhance(enhancedOnce, ref.head)
    display(enhancedTwice)
    enhancedTwice.count(_._2 == '1')
