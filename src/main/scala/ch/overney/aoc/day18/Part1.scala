package ch.overney.aoc.day18

import ch.overney.aoc.harness.AppWithInput
import ch.overney.aoc.harness.Point

object Part1
    extends AppWithInput(
      "day18",
      4140L,
      Seq(
        "1-1.txt" -> 548L,
        "1-1.1.txt" -> 1384L,
        "1-3.txt" -> 143L,
        "1-4.txt" -> 1384L,
        "1-5.txt" -> 445L,
        "1-6.txt" -> 791L,
        "1-7.txt" -> 1137L,
        "1-8.txt" -> 3488L,
        "1-2.txt" -> 3488L, // 3377L,
        "1-9.txt" -> 445L,
        "1-10.txt" -> 791L,
        "1-11.txt" -> 1137L,
        "1-12.txt" -> 1016L
      )
    ):

  override def solve(dataSet: Iterator[String]): Long =
    val inputs = dataSet.toSeq.map(Token.ize(_))
    val pairs = inputs.map(Pair(_)).collect { case p: Pair => p }
    println(pairs.mkString("\n"))
    val finalPair =
      pairs.tail.foldLeft(pairs.head.reduce()) { (acc, p) =>
        println("****")
        println(p)
        val newP = acc + p
        println("<<<<")
        newP.reduce()
      }
    println(s"---\n$finalPair")
    finalPair.magnitude
