package ch.overney.aoc.day21

import scala.annotation.tailrec

final case class PlayerState(id: Int, pos: Long, score: Long, winningScore: Long):
  import PlayerState._

  def forward(movement: Long): PlayerState =
    val newNewP = (pos + movement) % 10L
    val newPos = if newNewP == 0L then 10L else newNewP
    copy(pos = newPos, score + newPos)

  def move(die: DeterministicDie): PlayerState =
    val movement = (0L until 3L).map(_ => die.next).sum
    forward(movement)

  def moveDirac(): Seq[PlayerState] = DiracPosibilities.map(forward)

  def hasWon: Boolean = score >= winningScore

object PlayerState:
  private val DiracSides = 1L to 3L
  val DiracPosibilities: Seq[Long] =
    for {
      roll1 <- DiracSides
      roll2 <- DiracSides
      roll3 <- DiracSides
    } yield roll1 + roll2 + roll3

  def apply(input: String, winningScore: Long): PlayerState =
    val s"Player $id starting position: $pos" = input
    PlayerState(id.toInt, pos.toLong, 0L, winningScore)
