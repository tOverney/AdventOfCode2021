package ch.overney.aoc.day3

import ch.overney.aoc.harness.AppWithInput

object Part2 extends AppWithInput("day3", 230L):
  private def binToLong(binNum: String): Long =
    binNum.reverse.zipWithIndex
      .foldLeft(0L) { case (acc, (bit, pow)) => acc + (if (bit == '1') Math.pow(2, pow).toLong else 0) }

  override def solve(dataSet: Iterator[String]): Long =
    val words = dataSet.toSeq
    val wordSize = words.head.length

    def trim(loc: Int, input: Seq[String], least: Boolean = false): Seq[String] =
      val (zeroes, ones) = input.partition(w => w(loc) == '0')
      if ((zeroes.size > ones.size) != least) zeroes else ones

    @scala.annotation.tailrec
    def findBin(mostCommon: Seq[String], leastCommon: Seq[String], loc: Int = 0): (String, String) =
      (mostCommon, leastCommon) match
        case (Seq(oxygen), Seq(co2Scrubbber)) => (oxygen, co2Scrubbber)
        case (Seq(oxygen), least)             => findBin(Seq(oxygen), trim(loc, least, least = true), loc + 1)
        case (most, Seq(co2Scrubbber))        => findBin(trim(loc, most), Seq(co2Scrubbber), loc + 1)
        case (most, least)                    => findBin(trim(loc, most), trim(loc, least, least = true), loc + 1)

    val (oxygenBin, co2Bin) = findBin(words, words)

    binToLong(oxygenBin) * binToLong(co2Bin)
