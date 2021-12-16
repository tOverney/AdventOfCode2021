package ch.overney.aoc.day16

val binToHex =
  Map(
    "0000" -> '0',
    "0001" -> '1',
    "0010" -> '2',
    "0011" -> '3',
    "0100" -> '4',
    "0101" -> '5',
    "0110" -> '6',
    "0111" -> '7',
    "1000" -> '8',
    "1001" -> '9',
    "1010" -> 'A',
    "1011" -> 'B',
    "1100" -> 'C',
    "1101" -> 'D',
    "1110" -> 'E',
    "1111" -> 'F'
  )

val hexToBin =
  binToHex.map((k, v) => (v, k))

def binToLong(binNum: String): Long =
  binNum.reverse.zipWithIndex
    .foldLeft(0L) { case (acc, (bit, pow)) => acc + (if (bit == '1') Math.pow(2, pow).toLong else 0) }

@scala.annotation.tailrec
def parseLit(string: List[String], acc: String): (String, String) =
  string match
    case Nil                                  => (acc, "")
    case head :: tail if head.startsWith("1") => parseLit(tail, acc + head.drop(1))
    case head :: tail                         => (acc + head.drop(1), tail.mkString)
