package ch.overney.aoc.day24

import ch.overney.aoc.harness.AppWithInput
import ch.overney.aoc.day24.Value.Var

object Part2 extends AppWithInput("day24", "tst", skipSample = true):

  private lazy val Z: Var = Var("z")
  // Checking my input I can see that if Z goes past this max value we can never go back down to zero!
  // search for the last consecutive `div z 26` to adjust the pow if needed
  private lazy val TooBigZ = Math.pow(26, 4).toLong
  private lazy val DigitsToTry = 1L to 9L

  override def solve(dataSet: Iterator[String]): String =
    val instructions = dataSet.toSeq.map(Instruction(_))
    val sections = instructions.grouped(18).toIndexedSeq

    val memoize = collection.mutable.Map.empty[(Int, Long, Long), Option[Long]]

    def computeSection(id: Int, digit: Long, zStartValue: Long): Option[Long] =
      memoize.getOrElseUpdate(
        (id, digit, zStartValue), {
          val state = collection.mutable.Map(Z -> zStartValue)
          sections(id).map(_.execute(state, Iterator(digit)))
          val newZ = state(Z)
          Option.when(newZ < TooBigZ)(newZ)
        }
      )

    for {
      d1 <- DigitsToTry
      z1 <- computeSection(0, d1, 0L)
      d2 <- DigitsToTry
      z2 <- computeSection(1, d2, z1)
      _ = println(s"$d1$d2")
      d3 <- DigitsToTry
      z3 <- computeSection(2, d3, z2)
      d4 <- DigitsToTry
      z4 <- computeSection(3, d4, z3)
      d5 <- DigitsToTry
      z5 <- computeSection(4, d5, z4)
      d6 <- DigitsToTry
      z6 <- computeSection(5, d6, z5)
      d7 <- DigitsToTry
      z7 <- computeSection(6, d7, z6)
      d8 <- DigitsToTry
      z8 <- computeSection(7, d8, z7)
      d9 <- DigitsToTry
      z9 <- computeSection(8, d9, z8)
      d10 <- DigitsToTry
      z10 <- computeSection(9, d10, z9)
      d11 <- DigitsToTry
      z11 <- computeSection(10, d11, z10)
      d12 <- DigitsToTry
      z12 <- computeSection(11, d12, z11)
      d13 <- DigitsToTry
      z13 <- computeSection(12, d13, z12)
      d14 <- DigitsToTry
      z14 <- computeSection(13, d14, z13)
    } {
      val digits = Seq(d1, d2, d3, d4, d5, d6, d7, d8, d9, d10, d11, d12, d13, d14)

      if z14 == 0 then return digits.mkString
    }

    "done"
