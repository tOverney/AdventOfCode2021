package ch.overney.aoc.day22

import ch.overney.aoc.harness.AppWithInput
import ch.overney.aoc.harness.Point3d

object Part1 extends AppWithInput("day22", 590784L, Seq("tiny.txt" -> 39L)):

  private lazy val LookedAtRange = (-50L to 50L).toSet

  override def solve(dataSet: Iterator[String]): Long =
    val onPos = collection.mutable.HashSet.empty[Point3d]
    dataSet.foreach { input =>
      val cube = Cube(input)
      if cube.isInRange(LookedAtRange) then
        val pointsInCube = cube.allPos
        val size = cube.size
        require(pointsInCube.size.toLong == size, s"$cube shouldBe ${pointsInCube.size} is $size")
        if cube.isOn then onPos.addAll(pointsInCube)
        else onPos.filterInPlace(!pointsInCube.contains(_))
    }
    onPos.size
