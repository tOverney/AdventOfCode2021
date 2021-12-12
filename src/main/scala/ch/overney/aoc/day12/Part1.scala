package ch.overney.aoc.day12

import ch.overney.aoc.harness.AppWithInput

object Part1 extends AppWithInput("day12", 226L, Seq("tiny.txt" -> 10L, "small.txt" -> 19L), true):

  private def parseInput(caves: Set[Cave], input: String): Set[Cave] =
    val arr @ Array(from, to) = input.split("-").map(Cave.parse)
    val newSet = caves ++ arr
    newSet.find(_ == from).foreach(_.connections.addOne(to))
    newSet.find(_ == to).foreach(_.connections.addOne(from))
    newSet
  override def solve(dataSet: Iterator[String]): Long =
    val caves = dataSet.toSeq.foldLeft(Set[Cave]())(parseInput(_, _))

    def explore(curr: Cave, seen: Set[Cave], path: Seq[Cave]): Seq[Seq[Cave]] =
      val newPath = path :+ curr
      if curr.isEnd then Seq(newPath)
      else
        val newSeen = if curr.isSmall then seen + curr else seen
        curr.connections.toSeq.flatMap { conn =>
          if newSeen(conn) then Seq.empty
          else explore(caves.find(_ == conn).get, newSeen, newPath)
        }
    val paths = explore(caves.find(_.isStart).get, Set(), Seq())
    paths.size
