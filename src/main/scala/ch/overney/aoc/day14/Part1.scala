package ch.overney.aoc.day14

import ch.overney.aoc.harness.AppWithInput

object Part1 extends AppWithInput("day14", 1588):
  override def solve(dataSet: Iterator[String]): Int =
    val template = dataSet.next.toCharArray
    val rules = dataSet.toSeq.filterNot(_.isEmpty).map { case s"$a -> $n" => (a.head, a.tail.head) -> n.head }.toMap
    println(rules)

    def iterate(input: Seq[Char], remainingStep: Int): Seq[Char] =
      println(s"Step $remainingStep ${input.mkString("")}")
      if remainingStep == 0 then input
      else
        val expanded =
          input
            .sliding(2)
            .foldLeft(Seq(input.head)) { case (acc, Seq(curr, next)) =>
              acc :+ rules((curr, next)) :+ next
            }
        iterate(expanded, remainingStep - 1)

    val res = iterate(template, 10)
    val occurenceMap = res.groupBy(identity).mapValues(_.size)
    val mostCommon = occurenceMap.maxBy(_._2)._2
    val leastCommon = occurenceMap.minBy(_._2)._2

    mostCommon - leastCommon
