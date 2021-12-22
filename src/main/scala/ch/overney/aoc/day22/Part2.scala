package ch.overney.aoc.day22

import ch.overney.aoc.harness.AppWithInput
import ch.overney.aoc.harness.Point3d

object Part2
    extends AppWithInput("day22", 39769202357779L, Seq("tiny.txt" -> 39L, "samplep2.txt" -> 2758514936282235L)):

  override def solve(dataSet: Iterator[String]): Long =
    val cubes = dataSet.toSeq.map(Cube(_))

    val intersectedCubes =
      cubes.foldLeft(Seq[Cube]()) { case (seen, cube) =>
        println(s"adding $cube")
        val newCubes =
          for {
            otrCube <- seen
            overlapCube = cube.intersect(otrCube)
            if overlapCube.forall(_ != otrCube)
          } yield overlapCube.fold(Seq(otrCube))(otrCube.diff(_))
        newCubes.foreach(c => require(c.size <= 26))
        newCubes.flatten ++ Option.when(cube.isOn)(cube)
      }
    intersectedCubes.map(_.size).sum
