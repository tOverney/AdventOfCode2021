package ch.overney.aoc.day16

import ch.overney.aoc.harness.AppWithInput

object Part2
    extends AppWithInput(
      "day16",
      2021L,
      Seq(
        "2-1.txt" -> 3L,
        "2-2.txt" -> 54L,
        "2-3.txt" -> 7L,
        "2-4.txt" -> 9L,
        "2-5.txt" -> 1L,
        "2-6.txt" -> 0L,
        "2-7.txt" -> 0L,
        "2-8.txt" -> 1L
      )
    ):

  private def compareLongs(comp: (Long, Long) => Boolean)(longs: Seq[Long]): Long =
    if comp(longs(0), longs(1)) then 1L else 0L

  private def typeIdToFunc(typeId: Long): Seq[Long] => Long =
    typeId match {
      case 0L => _.sum
      case 1L => _.reduce(_ * _)
      case 2L => _.min
      case 3L => _.max
      case 5L => compareLongs(_ > _)
      case 6L => compareLongs(_ < _)
      case 7L => compareLongs(_ == _)
      case _  => sys.error("cannot happen")
    }

  override def solve(dataSet: Iterator[String]): Long =

    def parsePacket(
        input: String,
        acc: Seq[Long],
        func: Seq[Long] => Long,
        onlyOne: Boolean = false
    ): (Long, String) =
      println(s"Looking at: $input")
      if input.forall(_ == '0') then
        val res = func(acc)
        println(s"Bringing up $acc => $res")
        (res, input)
      else
        val (vRaw, tail) = input.splitAt(3)
        val version = binToLong(vRaw)
        println(s"version: $version")
        val (typeRaw, tail2) = tail.splitAt(3)
        val typeId = binToLong(typeRaw)
        println(s"typeId: $typeId")
        if typeId == 4L then
          val (litRaw, rest) = parseLit(tail2.grouped(5).toList, "")
          val lit = binToLong(litRaw)
          println(s"Literal: $lit")
          if onlyOne then (func(acc :+ lit), rest) else parsePacket(rest, acc :+ lit, func)
        else
          val (lengthTypeId, tailTail) = tail2.splitAt(1)
          if lengthTypeId == "0" then
            val (lenRaw, t) = tailTail.splitAt(15)
            val length = binToLong(lenRaw)
            println(s"length: $length")
            val (subPackets, others) = t.splitAt(length.toInt)
            var rem = subPackets
            val subPacketsResult = collection.mutable.ArrayBuffer[Long]()
            while (rem.nonEmpty) {
              val (res, newRem) = parsePacket(rem, Nil, _(0), onlyOne = true)
              rem = newRem
              subPacketsResult.addOne(res)
            }
            val subPackRes = subPacketsResult.toSeq
            println(subPackRes)
            val res = typeIdToFunc(typeId)(subPackRes)
            println(s"$subPackRes ** $res")
            (res, others)
          else
            val (numRaw, t) = tailTail.splitAt(11)
            val num = binToLong(numRaw)
            println(s"num: $num")
            val (tot, ttt) = (1L to num).foldLeft((Seq[Long](), t)) { case ((resAcc, rest), iter) =>
              println(s"#$iter for $version")
              val (res, tt) = parsePacket(rest, Nil, _(0), onlyOne = true)
              (resAcc :+ res, tt)
            }
            val res = typeIdToFunc(typeId)(tot)
            println(s"$tot -> $res")
            (res, ttt)

    val input = dataSet.next
    println(input)
    val cleanedInput = input.flatMap(hexToBin)
    parsePacket(cleanedInput, Nil, _.sum)._1
