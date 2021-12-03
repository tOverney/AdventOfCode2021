package ch.overney.aoc.harness

import scala.io.Source

trait AppWithInput[A](id: String, sampleAnswer: A, extraPaths: IndexedSeq[String] = IndexedSeq.empty) extends App:

  def solve(dataSet: Iterator[String]): A

  val paths = List("sample.txt", "input.txt") ++ extraPaths
  val sampleDataSet :: dataSets =
    paths.map(p => Source.fromInputStream(getClass.getClassLoader.getResourceAsStream(s"$id/$p")).getLines)

  val sampleResult = solve(sampleDataSet)
  if (sampleResult != sampleAnswer) {
    sys.error(
      s"""Sample dataset does not yield the expected result
         |found=$sampleResult
         |expected=$sampleAnswer""".stripMargin
    )
  } else {
    println(s"Sample yielded the correct result ($sampleAnswer)")
  }

  for {
    (dataSet, idx) <- dataSets.zipWithIndex
    result = solve(dataSet)
  } println(s"Dataset#$idx result: $result")
