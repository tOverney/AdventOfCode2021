package ch.overney.aoc.day21

import scala.annotation.tailrec

final case class PlayerState(id: Int, pos: Long, score: Long, winningScore: Long):
  import PlayerState._

  def move(die: DeterministicDie): PlayerState =
    val newPos =
      (0L until 3L).fold(pos) { (newPos, _) =>
        val newNewP = (newPos + die.next) % 10L
        if newNewP == 0L then 10L else newNewP
      }
    val newScore = score + newPos
    println(s"#$id: $newPos, $newScore")
    copy(pos = newPos, newScore)

  def moveDirac(): Seq[PlayerState] =
    DiracPosibilities.map { rolls =>
      val newPos =
        rolls.fold(pos) { (newPos, roll) =>
          val newNewP = (newPos + roll) % 10L
          if newNewP == 0L then 10L else newNewP
        }
      copy(pos = newPos, score + newPos)
    }

  def hasWon: Boolean = score >= winningScore

object PlayerState:
  private val DiracSides = 1L to 3L
  val DiracPosibilities: Seq[Seq[Long]] =
    for {
      roll1 <- DiracSides
      roll2 <- DiracSides
      roll3 <- DiracSides
    } yield Seq(roll1, roll2, roll3)

  def apply(input: String, winningScore: Long): PlayerState =
    val s"Player $id starting position: $pos" = input
    PlayerState(id.toInt, pos.toLong, 0L, winningScore)
