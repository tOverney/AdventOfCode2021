package ch.overney.aoc.day10

enum Chunk(val opening: Char, val closing: Char, val corruptionCost: Long, val closingCost: Long):
  case Parentheses extends Chunk('(', ')', 3L, 1L)
  case SquareBrackets extends Chunk('[', ']', 57L, 2L)
  case Braces extends Chunk('{', '}', 1197L, 3L)
  case Chevrons extends Chunk('<', '>', 25137L, 4L)

object Chunk:
  def unapply(char: Option[Char]): Option[Chunk] =
    char.flatMap(c => Chunk.values.find(_.opening == c))

  def byClosing(char: Char): Chunk =
    Chunk.values.find(_.closing == char).getOrElse(sys.error(s"Unexpected char: $char"))
