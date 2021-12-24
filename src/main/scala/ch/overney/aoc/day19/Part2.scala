package ch.overney.aoc.day19

import ch.overney.aoc.harness.AppWithInput
import ch.overney.aoc.harness.Point3d

import scala.annotation.tailrec

object Part2 extends AppWithInput("day19", 3621L):

  override def solve(dataSet: Iterator[String]): Long =
    val scanners = Scanner.all(dataSet.toList)
    val referenceScanner = scanners.head

    @tailrec
    def iterate(toTranslate: Seq[Scanner], toUseAsRef: Map[Scanner, Point3d], solved: Seq[Point3d]): Seq[Point3d] =
      if toUseAsRef.isEmpty then solved
      else
        val (current, currentPos) = toUseAsRef.head
        val (translated, stillToTranslate) =
          toTranslate.foldLeft((toUseAsRef.tail, Seq.empty[Scanner])) { case ((accDone, accStill), possibleOverlap) =>
            current.translateScanner(possibleOverlap).fold((accDone, accStill :+ possibleOverlap)) {
              case (shiftedScanner, offset) =>
                (accDone + ((shiftedScanner, offset)), accStill)
            }
          }
        println(s"${current.id} ---> ${translated.map(_._1.id)}")
        iterate(stillToTranslate, translated, solved :+ currentPos)

    val reference = scanners.head
    val scannersPos = iterate(scanners.tail, Map(reference -> Point3d(0L, 0L, 0L)), Seq())
    println(scannersPos)
    val distances =
      scannersPos.combinations(2).toSeq.map { case Seq(first, second) => first.distanceTo(second) }

    distances.max
