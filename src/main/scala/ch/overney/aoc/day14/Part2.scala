package ch.overney.aoc.day14

import ch.overney.aoc.harness.AppWithInput
import ch.overney.aoc.harness.Point

import scala.annotation.tailrec

object Part2 extends AppWithInput("day14", 2188189693529L):

  private def addToKey[A](map: Map[A, Long], key: A, value: Long): Map[A, Long] =
    map.updatedWith(key)(_.fold(Some(value))(v => Some(v + value)))

  override def solve(dataSet: Iterator[String]): Long =
    val template = dataSet.next.toCharArray
    val rules = dataSet.toSeq.filterNot(_.isEmpty).map { case s"$a -> $n" => (a.head, a.tail.head) -> n.head }.toMap
    println(rules)

    def iterate(input: Map[(Char, Char), Long], remainingStep: Int): Map[(Char, Char), Long] =
      println(s"Step $remainingStep")
      if remainingStep == 0 then input
      else
        val expanded =
          input
            .foldLeft(Map.empty[(Char, Char), Long]) { case (acc, (curr, next) -> count) =>
              val inserted = rules((curr, next))
              addToKey(addToKey(acc, (curr, inserted), count), (inserted, next), count)
            }
        iterate(expanded, remainingStep - 1)

    val input = template.sliding(2).toSeq.groupBy { case Array(a, b) => (a, b) }.mapValues(_.size.toLong).toMap
    val res = iterate(input, 40)
    val notReduced = res.toSeq.flatMap { case ((a, b), count) => Seq(a -> count, b -> count) }
    println(notReduced)
    val occurenceMapRaw: Map[Char, Long] = notReduced.groupMapReduce(_._1)(_._2)(_ + _)
    val occurenceMap = addToKey(addToKey(occurenceMapRaw, template.head, 1L), template.last, 1L)
    println(occurenceMap)
    val mostCommon = occurenceMap.maxBy(_._2)._2 / 2
    val leastCommon = occurenceMap.minBy(_._2)._2 / 2

    println(s"$mostCommon, $leastCommon")

    mostCommon - leastCommon
