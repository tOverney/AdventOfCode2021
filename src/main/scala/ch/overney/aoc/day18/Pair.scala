package ch.overney.aoc.day18

import ch.overney.aoc.day18.Token.*
import scala.annotation.tailrec

final case class Pair(left: Long | Pair, right: Long | Pair, nestingLevel: Long):
  import Pair._

  @tailrec
  def reduce(previous: Pair = this): Pair =
    println(previous)
    val exploded = explode(previous)
    if exploded == previous then
      val splitted = split(previous)
      if splitted == previous then splitted
      else reduce(splitted)
    else reduce(exploded)

  def +(other: Pair): Pair = Pair(nest(this), nest(other), 0)
  def magnitude: Long = mult(3L, left) + mult(2L, right)

  def withLeft(newLeft: Long | Pair): Pair = Pair(newLeft, right, nestingLevel)
  def withRight(newRight: Long | Pair): Pair = Pair(left, newRight, nestingLevel)

  override def toString: String =
    s"${if nestingLevel == 4 then "{" else "["}${negativeToX(left)},${negativeToX(right)}${if nestingLevel == 4 then "}"
    else "]"}"

object Pair:
  def negativeToX(p: Long | Pair): Any =
    if p == -1L then "X" else p

  private def mult(factor: Long, elem: Long | Pair): Long =
    factor * (
      elem match
        case num: Long  => num
        case pair: Pair => pair.magnitude
    )

  private def nest(elem: Long | Pair): Long | Pair =
    elem match
      case num: Long  => num
      case pair: Pair => Pair(nest(pair.left), nest(pair.right), pair.nestingLevel + 1)

  def explode(p: Pair): Pair =
    def findExplosion(p: Pair): (Option[(Long, Long)], Pair) =
      def explodeOrRecurse(side: Long | Pair, modifyPair: Long | Pair => Pair): (Option[(Long, Long)], Pair) =
        side match
          case other @ Pair(leftNum: Long, rightNum: Long, nLevel) if nLevel >= 4 =>
            println(s"exploding $other")
            (Some((leftNum, rightNum)), modifyPair(-1L))
          case num: Long => (None, modifyPair(num))
          case other: Pair =>
            val (nums, pair) = findExplosion(other)
            (nums, modifyPair(pair))
      val left @ (_, reducedLeft) = explodeOrRecurse(p.left, p.withLeft)
      if reducedLeft != p then left
      else explodeOrRecurse(p.right, p.withRight)

    findExplosion(p) match
      case (None, reduced)             => reduced
      case (Some((left, right)), newP) => patch(newP, left, right)

  def split(p: Pair): Pair =
    def splitOrRecurse(side: Long | Pair, modifyPair: Long | Pair => Pair): Pair =
      side match
        case num: Long if num >= 10L =>
          println(s"Splitting $num")
          modifyPair(Pair(num / 2, (num + 1) / 2, p.nestingLevel + 1))
        case other: Long => p
        case other: Pair => modifyPair(split(other))

    val splittedLeft = splitOrRecurse(p.left, p.withLeft)
    if splittedLeft == p then splitOrRecurse(p.right, p.withRight)
    else splittedLeft

  def apply(
      tokens: Seq[Token],
      nestingLevel: Int = 0
  ): Long | Pair =
    tokens match
      case Seq(Lit(num)) => num
      case _ =>
        val toks = if tokens.startsWith(Seq(Open)) then tokens.tail.init else tokens
        var stack: Long = 0L
        val lhs =
          toks.takeWhile { t =>
            if t == Comma && stack == 0L then false
            else if t == Open then
              stack += 1
              true
            else if t == Close then
              stack -= 1
              true
            else true
          }
        val rhs = toks.drop(lhs.size + 1)
        val left = apply(lhs, nestingLevel + 1)
        val right = apply(rhs, nestingLevel + 1)
        Pair(left, right, nestingLevel)

  def patch(p: Pair, leftNum: Long, rightNum: Long): Pair =
    val tokens = Token.ize(p.toString)
    val (before, after) = tokens.splitAt(tokens.indexOf(Explosion))
    val patchedBefore = Token.patchFirstLit(before.reverse, leftNum).reverse
    val patchedAfter = Token.patchFirstLit(after.tail, rightNum)
    val patchedTokens: Seq[Token] = (patchedBefore :+ Lit(0L)) ++ patchedAfter
    apply(patchedTokens, 0).asInstanceOf[Pair]
