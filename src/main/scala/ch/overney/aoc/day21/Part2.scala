package ch.overney.aoc.day21

import ch.overney.aoc.harness.AppWithInput

object Part2 extends AppWithInput("day21", 444356092776315L):

  override def solve(dataSet: Iterator[String]): Long =
    val p1 = PlayerState(dataSet.next, 21)
    val p2 = PlayerState(dataSet.next, 21)

    val die = new DeterministicDie()

    var p1Won = 0L
    var p2Won = 0L

    def notePlayerVictory(p: PlayerState, count: Long): Unit =
      if p.id == 1 then p1Won += count
      else p2Won += count

    def roll(playerToMove: Map[PlayerState, Long], otherPlayer: Map[PlayerState, Long]): Unit =
      val allPoss =
        for {
          (player, count) <- playerToMove.toSeq
          movedPlayer <- player.moveDirac()
        } yield
          if movedPlayer.hasWon then
            notePlayerVictory(movedPlayer, count * otherPlayer.values.sum)
            Nil
          else Seq((movedPlayer, count))
      val movedPlayer = allPoss.flatten.groupMapReduce(_._1)(_._2)(_ + _)
      if movedPlayer.isEmpty then println("Done")
      else roll(otherPlayer, movedPlayer)

    roll(Map(p1 -> 1L), Map(p2 -> 1L))

    Math.max(p1Won, p2Won)
