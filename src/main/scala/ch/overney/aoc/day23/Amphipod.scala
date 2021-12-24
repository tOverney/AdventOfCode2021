package ch.overney.aoc.day23

enum Amphipod(energyForStep: Long):
  case Amber extends Amphipod(1L)
  case Bronze extends Amphipod(10L)
  case Copper extends Amphipod(100L)
  case Desert extends Amphipod(1000L)

object Amphipod:
  def apply(char: Char): Amphipod =
    char match
      case 'A' => Amber
      case 'B' => Bronze
      case 'C' => Copper
      case 'D' => Desert
      case _   => sys.error("no")
