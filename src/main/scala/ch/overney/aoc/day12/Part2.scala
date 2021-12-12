package ch.overney.aoc.day12

import ch.overney.aoc.harness.AppWithInput
import ch.overney.aoc.harness.Point

import scala.annotation.tailrec

object Part2 extends AppWithInput("day12", 3509L, Seq("tiny.txt" -> 36L, "small.txt" -> 103L)):
  private def parseInput(caves: Set[Cave], input: String): Set[Cave] =
    val arr @ Array(from, to) = input.split("-").map(Cave.parse)
    val newSet = caves ++ arr
    newSet.find(_ == from).foreach(_.connections.addOne(to))
    newSet.find(_ == to).foreach(_.connections.addOne(from))
    newSet
  override def solve(dataSet: Iterator[String]): Long =
    val caves = dataSet.toSeq.foldLeft(Set[Cave]())(parseInput(_, _))

    def explore(curr: Cave, seenOnce: Set[Cave], seen: Set[Cave], path: Seq[Cave]): Seq[Seq[Cave]] =
      val newPath = path :+ curr
      if curr.isEnd then Seq(newPath)
      else
        val newSeenPoss =
          if curr.isStart then Seq((seenOnce, seen + curr))
          else if curr.isSmall && seenOnce.nonEmpty then Seq((seenOnce, seen + curr))
          else if curr.isSmall then Seq((seenOnce + curr, seen), (seenOnce, seen + curr))
          else Seq((seenOnce, seen))
        curr.connections.toSeq.flatMap { conn =>
          newSeenPoss.flatMap { case (newSeenOnce, newSeen) =>
            if newSeen(conn) then Seq.empty
            else explore(caves.find(_ == conn).get, newSeenOnce, newSeen, newPath)
          }
        }
    val paths = explore(caves.find(_.isStart).get, Set(), Set(), Seq())
    paths.distinct.size
