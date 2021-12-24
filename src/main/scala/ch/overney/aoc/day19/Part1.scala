package ch.overney.aoc.day19

import ch.overney.aoc.harness.AppWithInput
import ch.overney.aoc.harness.Point3d

import scala.annotation.tailrec

object Part1 extends AppWithInput("day19", 79):

  override def solve(dataSet: Iterator[String]): Int =
    val scanners = Scanner.all(dataSet.toList)
    val referenceScanner = scanners.head

    @tailrec
    def iterate(toTranslate: Seq[Scanner], toUseAsRef: Seq[Scanner], solved: Seq[Scanner]): Int =
      if toUseAsRef.isEmpty then solved.flatMap(_.beacons).toSet.size
      else
        val current = toUseAsRef.head
        val (translated, stillToTranslate) =
          toTranslate.foldLeft((Seq.empty[Scanner], Seq.empty[Scanner])) {
            case ((accDone, accStill), possibleOverlap) =>
              current.translateScanner(possibleOverlap).fold((accDone, accStill :+ possibleOverlap)) {
                case (shiftedScanner, _) =>
                  (accDone :+ shiftedScanner, accStill)
              }
          }
        println(s"${current.id} ---> ${translated.map(_.id)}")
        iterate(stillToTranslate, toUseAsRef.tail ++ translated, solved :+ current)

    val reference = scanners.head
    iterate(scanners.tail, Seq(reference), Seq())
