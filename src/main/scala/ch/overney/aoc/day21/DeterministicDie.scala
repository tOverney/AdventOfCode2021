package ch.overney.aoc.day21

class DeterministicDie(private var _nextValue: Long = 1L):
  private var _numRolls: Long = 0L
  def numberOfRolls: Long = _numRolls
  def next: Long =
    val value = _nextValue
    _numRolls += 1L
    _nextValue = if value == 100L then 1L else value + 1L
    value
