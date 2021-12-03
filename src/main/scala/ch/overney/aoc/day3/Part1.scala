package ch.overney.aoc.day3

import ch.overney.aoc.harness.AppWithInput

object Part1 extends AppWithInput("day3", 198L):

  private def binToLong(binNum: String): Long =
    binNum.reverse.zipWithIndex
      .foldLeft(0L) { case (acc, (bit, pow)) => acc + (if (bit == '1') Math.pow(2, pow).toLong else 0) }

  override def solve(dataSet: Iterator[String]): Long =
    val words = dataSet.toSeq
    val wordSize = words.head.length
    val thresholdPoint = words.size / 2
    val outputBin =
      for {
        x <- 0 until wordSize
        zeroCount = words.count(w => w(x) == '0')
        gammaAndEpsilonBit = if (zeroCount > thresholdPoint) ('0', '1') else ('1', '0')
      } yield gammaAndEpsilonBit

    val (gammaBin, epsilonBin) = outputBin.unzip

    binToLong(gammaBin.mkString) * binToLong(epsilonBin.mkString)
