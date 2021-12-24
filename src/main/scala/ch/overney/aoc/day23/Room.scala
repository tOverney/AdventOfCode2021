package ch.overney.aoc.day23

final case class Room(targetType: Amphipod, amphipods: Seq[Amphipod]):
  val hasReachedTarget: Boolean = amphipods.size == 2 && amphipods.forall(targetType == _)

object Room:
  def apply(targetType: Amphipod, top: Char, bottom: Char): Room =
    Room(targetType, Seq(Amphipod(top), Amphipod(bottom)))
