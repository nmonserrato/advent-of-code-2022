package com.adventofcode.twentywentytwo

import scala.collection.mutable
import scala.io.Source
import scala.util.Using

object Day03 {
  def main(args: Array[String]): Unit = {
    println(findTotalItemsPriorities)
    println(findTotalGroupPriorities)
  }

  private def findTotalGroupPriorities: Long = {
    var bs1: mutable.BitSet = null
    var bs2: mutable.BitSet = null

    Using(Source.fromResource("inputs/day03.txt")) { source =>
      source.getLines().zipWithIndex.map { case (line, ln) =>
        ln % 3 match {
          case 0 => bs1 = fillBitSet(line); 0
          case 1 => bs2 = fillBitSet(line); 0
          case 2 => line.find(c => bs1(c.priority) && bs2(c.priority)).get.priority
        }
      }.sum
    }.get
  }

  private def findTotalItemsPriorities: Long = {
    Using(Source.fromResource("inputs/day03.txt")) { source =>
      source.getLines().map(findDuplicateLetterPriority).sum
    }.get
  }

  private def findDuplicateLetterPriority(line: String): Int = {
    val half = line.length / 2
    val cache = fillBitSet(line, half)
    (line.length - 1 to half by -1)
      .map(i => line.charAt(i).priority)
      .find(cache(_)).get
  }

  private def fillBitSet(line: String, len: Int = -1): mutable.BitSet = {
    val until = if (len == -1) line.length else len
    val bs = new mutable.BitSet(initSize = 52)
    line.take(until).foreach(c => bs += c.priority)
    bs
  }

  implicit class CharPriority(ch: Char) {
    val priority: Int = if (ch > 97) ch - 96 else ch - 65 + 27
  }
}
