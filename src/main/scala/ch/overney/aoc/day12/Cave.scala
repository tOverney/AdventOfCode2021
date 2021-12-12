package ch.overney.aoc.day12

import scala.collection.mutable.Set

enum Cave(val name: String, val isSmall: Boolean, val connections: Set[Cave] = Set.empty):
  case Small(nme: String) extends Cave(nme, true)
  case Big(nme: String) extends Cave(nme, false)

  override def equals(other: Any): Boolean =
    other match
      case c: Cave => c.name == name
      case _       => false

  val isStart: Boolean = name == "start"
  val isEnd: Boolean = name == "end"

  def toStringWithConn: String = s"$name -> ${connections.map(_.name)}"

object Cave:
  def parse(name: String): Cave =
    if (name.forall(_.isUpper)) Big(name) else Small(name)
