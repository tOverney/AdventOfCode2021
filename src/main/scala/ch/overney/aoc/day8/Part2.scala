package ch.overney.aoc.day8

import ch.overney.aoc.harness.AppWithInput
import ch.overney.aoc.harness.Point

object Part2 extends AppWithInput("day8", 61229L):

  private lazy val UniquePatternLengthToNum =
    Map(2 -> 1, 3 -> 7, 4 -> 4, 7 -> 8)

  private def parseClockNums(nums: String): Seq[Set[Char]] =
    nums.split(" ").map(_.toSet).toSeq

  private def decodeInput(input: String): Map[Set[Char], Int] =
    val sets = parseClockNums(input)
    val (uniquePatternsSeq, notUnique) =
      sets.partitionMap { chars =>
        UniquePatternLengthToNum
          .get(chars.size)
          .fold[Either[(Int, Set[Char]), Set[Char]]](Right(chars))(num => Left(num -> chars))
      }
    // 1, 7, 4, 8 are solved
    val solvedPatterns = collection.mutable.Map.from(uniquePatternsSeq)

    def find(num: Int, remaining: Seq[Set[Char]], condition: Set[Char] => Boolean) =
      val (Seq(numPattern), newRemaining) = remaining.partition(condition)
      solvedPatterns.addOne(num -> numPattern)
      newRemaining

    val top = solvedPatterns(7).diff(solvedPatterns(1))
    val middle = notUnique.filter(_.size == 5).fold(solvedPatterns(4))(_.intersect(_)).head

    val remAfter0 = find(0, notUnique, p => p.size == 6 && !p(middle))

    val topRight =
      val Seq(nineOr6, sixOr9) = remAfter0.filter(_.size == 6)
      nineOr6.diff(sixOr9).union(sixOr9.diff(nineOr6)).intersect(solvedPatterns(1)).head

    val remAfter6 = find(6, remAfter0, _ == solvedPatterns(8).-(topRight))
    val remAfter9 = find(9, remAfter6, _.size == 6)
    val remAfter3 = find(3, remAfter9, p => solvedPatterns(1).forall(p))
    val remAfter2 = find(2, remAfter3, _.contains(topRight))
    val Seq() = find(5, remAfter2, _ => true)

    solvedPatterns.map { case (k, v) => (v, k) }.toMap

  override def solve(dataSet: Iterator[String]): Long =
    dataSet.foldLeft(0L) { case (acc, s"$input | $output") =>
      val decoder = decodeInput(input)
      val decodedOutput = parseClockNums(output).map(decoder(_)).mkString
      acc + decodedOutput.toLong
    }
