package ch.overney.aoc.harness

import scala.io.Source

trait AppWithInput[A](
    id: String,
    sampleAnswer: A,
    extraSamples: Seq[(String, A)] = Seq.empty,
    skipValidation: Boolean = false
) extends App:

  def solve(dataSet: Iterator[String]): A

  val (extraPaths, extraAnswers) = extraSamples.unzip
  val paths = List("input.txt") ++ extraPaths ++ List("sample.txt")
  val dataSet :: sampleDataSets =
    paths.map(p => Source.fromInputStream(getClass.getClassLoader.getResourceAsStream(s"$id/$p")).getLines)

  for {
    ((sampleDs, sampleAnswr), idx) <- sampleDataSets.zip(extraAnswers :+ sampleAnswer).zipWithIndex
  } {
    val sampleResult = solve(sampleDs)
    if !skipValidation && sampleResult != sampleAnswr then
      sys.error(
        s"""Sample dataset#$idx does not yield the expected result
           |found=$sampleResult
           |expected=$sampleAnswr""".stripMargin
      )
    else println(s"Sample yielded the correct result ($sampleAnswr)")
  }

  val result = solve(dataSet)
  println(s"Dataset result is: $result")
