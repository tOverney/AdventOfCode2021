package ch.overney.aoc.day19

import ch.overney.aoc.harness.Point3d
import scala.annotation.tailrec

final case class Scanner(id: Int, beacons: Seq[Point3d]):
  import Scanner._

  def allTranslations: Seq[Scanner] =
    beacons.map(_.possibleTranslations).transpose.map(translated => Scanner(id, translated))

  def shifted(offset: Point3d): Scanner = Scanner(id, beacons.map(_ + offset))

  val diffs: Set[Point3d] =
    (
      for
        b1 <- beacons
        b2 <- beacons
      yield b1 - b2
    ).toSet

  def countIntersects(other: Scanner, offset: Point3d): Int =
    other.beacons.map(_ + offset).toSet.intersect(beacons.toSet).size

  private def seemToOverlap(second: Scanner): Boolean =
    second.diffs.intersect(diffs).size > OverlapTarget * (OverlapTarget - 1)

  private def translationKey(target: Scanner): Option[(Scanner, Point3d)] =
    for
      first <- beacons
      second <- target.beacons
      offset = first - second
      if countIntersects(target, offset) >= OverlapTarget
    do return Some((target.shifted(offset), offset))
    None

  def translateScanner(other: Scanner): Option[(Scanner, Point3d)] =
    @tailrec
    def findOverlap(toCheck: Seq[Scanner]): Option[(Scanner, Point3d)] =
      toCheck.headOption match
        case None => None
        case Some(candidate) if seemToOverlap(candidate) =>
          translationKey(candidate) match
            case matched @ Some(_) => return matched
            case None              => findOverlap(toCheck.tail)
        case Some(_) => findOverlap(toCheck.tail)

    findOverlap(other.allTranslations)

object Scanner:
  private val OverlapTarget = 12

  def apply(label: String, rawBeacons: Seq[String]): Scanner =
    val s"--- scanner $id ---" = label
    val beacons = rawBeacons.filterNot(_.isEmpty).map(Point3d(_))
    Scanner(id.toInt, beacons)

  @tailrec
  def all(input: List[String], acc: Seq[Scanner] = Nil): Seq[Scanner] =
    input match {
      case Nil => acc
      case head :: tail if head.startsWith("---") =>
        val idx = tail.indexWhere(_.startsWith("---"))
        val (beacons, nextScanners) = if idx == -1 then (tail, Nil) else tail.splitAt(idx)
        all(nextScanners, acc :+ Scanner(head, beacons))
      case othr => sys.error(s"Wrong $othr")
    }
