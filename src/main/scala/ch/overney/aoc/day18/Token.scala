package ch.overney.aoc.day18

enum Token:
  case Open extends Token
  case Close extends Token
  case Comma extends Token
  case Explosion extends Token
  case Lit(num: Long) extends Token

object Token:
  private val Number = """(\d+)(.*)""".r
  @scala.annotation.tailrec
  def ize(raw: String, acc: Seq[Token] = Seq()): Seq[Token] =
    raw match
      case ""                => acc
      case s"X$tail"         => ize(tail, acc :+ Explosion)
      case s"[$tail"         => ize(tail, acc :+ Open)
      case s"{$tail"         => ize(tail, acc :+ Open)
      case s"]$tail"         => ize(tail, acc :+ Close)
      case s"}$tail"         => ize(tail, acc :+ Close)
      case s",$tail"         => ize(tail, acc :+ Comma)
      case Number(num, tail) => ize(tail, acc :+ Lit(num.toLong))

  def patchFirstLit(tokens: Seq[Token], numToAdd: Long): Seq[Token] =
    var foundLit = false
    tokens.map {
      case Lit(num) if !foundLit =>
        foundLit = true
        Lit(num + numToAdd)
      case other => other
    }
