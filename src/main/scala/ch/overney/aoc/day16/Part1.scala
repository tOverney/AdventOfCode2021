package ch.overney.aoc.day16

import ch.overney.aoc.harness.AppWithInput
import ch.overney.aoc.harness.Point

object Part1
    extends AppWithInput(
      "day16",
      6L,
      Seq(
        "explained.txt" -> 9L,
        "first.txt" -> 16L,
        "second.txt" -> 12L,
        "third.txt" -> 23L,
        "fourth.txt" -> 31L
      )
    ):
  override def solve(dataSet: Iterator[String]): Long =

    def parsePacket(input: String, acc: Long): Long =
      if input.isEmpty then return acc

      println(s"Looking at: $input")
      val (vRaw, tail) = input.splitAt(3)
      val version = binToLong(vRaw)
      println(s"version: $version")
      val (typeRaw, tail2) = tail.splitAt(3)
      val typeId = binToLong(typeRaw)
      println(s"typeId: $typeId")
      val newAcc = version + acc
      if typeId == 4L then
        val (litRaw, rest) = parseLit(tail2.grouped(5).toList, "")
        val lit = binToLong(litRaw)
        println(s"Literal: $lit")
        parsePacket(rest, newAcc)
      else
        val (lengthTypeId, tailTail) = tail2.splitAt(1)
        if lengthTypeId == "0" then
          val (lenRaw, t) = tailTail.splitAt(15)
          val length = binToLong(lenRaw)
          println(s"length: $length")
          val (subPackets, others) = t.splitAt(length.toInt)
          parsePacket(others, parsePacket(subPackets, newAcc))
        else
          val (numRaw, t) = tailTail.splitAt(11)
          val num = binToLong(numRaw)
          println(s"num: $num")
          parsePacket(t, newAcc)

    val cleanedInput = dataSet.next.flatMap(hexToBin)
    parsePacket(cleanedInput, 0L)
