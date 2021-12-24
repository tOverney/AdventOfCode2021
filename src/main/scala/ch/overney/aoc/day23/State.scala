package ch.overney.aoc.day23

import ch.overney.aoc.day23.Amphipod.*

final case class State(
    rooms: Seq[Room],
    leftA: Option[Amphipod] = None,
    aInterB: Option[Amphipod] = None,
    bInterC: Option[Amphipod] = None,
    cInterD: Option[Amphipod] = None,
    rightD: Option[Amphipod] = None
):
  val isSorted: Boolean = rooms.forall(_.hasReachedTarget)

object State:
  def apply(in: Array[String]): State =
    val input = in.take(5)
    val rooms =
      Seq(
        Room(Amber, input(2)(3), input(3)(3)),
        Room(Bronze, input(2)(5), input(3)(5)),
        Room(Copper, input(2)(7), input(3)(7)),
        Room(Desert, input(2)(9), input(3)(9))
      )

    State(rooms)
