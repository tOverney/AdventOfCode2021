package ch.overney.aoc.day25

import ch.overney.aoc.harness.Point

enum SeaCucumber(val isEastBound: Boolean, val char: Char):
  case EastBound extends SeaCucumber(true, '>')
  case SouthBound extends SeaCucumber(false, 'v')

object SeaCucumber:
  def unapply(c: Char): Option[SeaCucumber] =
    c match
      case 'v' => Some(SouthBound)
      case '>' => Some(EastBound)
      case _   => None

  def parseAll(input: Seq[String]): Map[Point, SeaCucumber] =
    (
      for
        (row, y) <- input.zipWithIndex
        (cell, x) <- row.zipWithIndex
        cucumber <- SeaCucumber.unapply(cell)
      yield Point(x, y) -> cucumber
    ).toMap
