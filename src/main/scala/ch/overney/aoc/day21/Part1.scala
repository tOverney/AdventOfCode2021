package ch.overney.aoc.day21

import ch.overney.aoc.harness.AppWithInput

import scala.annotation.tailrec

object Part1 extends AppWithInput("day21", 739785L):

  override def solve(dataSet: Iterator[String]): Long =
    val p1 = PlayerState(dataSet.next, 1000)
    val p2 = PlayerState(dataSet.next, 1000)

    val die = new DeterministicDie()

    @tailrec
    def roll(playerToMove: PlayerState, otherPlayer: PlayerState): (PlayerState, PlayerState) =
      val movedPlayer = playerToMove.move(die)
      if movedPlayer.hasWon then (movedPlayer, otherPlayer)
      else roll(otherPlayer, movedPlayer)

    val (winner, loser) = roll(p1, p2)

    loser.score * die.numberOfRolls
