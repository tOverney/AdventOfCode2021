package ch.overney.aoc.day6

import ch.overney.aoc.harness.AppWithInput
import ch.overney.aoc.harness.Point

object Part1 extends AppWithInput("day6", 5934L):
  private lazy val NewFishInitialStatus: Int = 8
  private lazy val OldFishInitialStatus: Int = 6

  private def addToKey(map: Map[Int, Long], key: Int, increment: Long): Map[Int, Long] =
    map.updatedWith(key)(_.map(_ + increment).orElse(Some(increment)))

  override def solve(dataSet: Iterator[String]): Long =
    val startingFishes: Map[Int, Long] = dataSet.next.split(",").map(_.toInt).groupMapReduce(identity)(_ => 1L)(_ + _)

    @scala.annotation.tailrec
    def dailyGen(startingState: Map[Int, Long], remainingDays: Long): Map[Int, Long] =
      if (remainingDays == 0) startingState
      else
        val newAcc =
          startingState.foldLeft(Map.empty[Int, Long]) { case (acc, (fishStatus, count)) =>
            if (fishStatus == 0) addToKey(addToKey(acc, OldFishInitialStatus, count), NewFishInitialStatus, count)
            else addToKey(acc, fishStatus - 1, count)

          }
        dailyGen(newAcc, remainingDays - 1)

    dailyGen(startingFishes, 80).values.sum
