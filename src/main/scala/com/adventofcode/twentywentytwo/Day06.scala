package com.adventofcode.twentywentytwo

import scala.collection.mutable
import scala.io.Source
import scala.util.Using

object Day06 {

  def main(args: Array[String]): Unit = {
    println(findMarkerOfSize(4))
    println(findMarkerOfSize(14))
  }

  private def findMarkerOfSize(size: Int): Int = {
    val input = Using(Source.fromResource("inputs/day06.txt"))(_.getLines().next()).get
    val map = new CountMapWithDistinctCount
    var (l, r) = (0, size - 1)
    (l to r).foreach(i => map.add(input.charAt(i)))

    while (map.numDistinct != size && r < input.length - 1) {
      map.remove(input.charAt(l))
      l += 1
      r += 1
      map.add(input.charAt(r))
    }

    r + 1
  }
}

class CountMapWithDistinctCount {
  private val data = new mutable.HashMap[Char, Int]()
  ('a' to 'z').foreach(data.put(_, 0))

  var numDistinct = 0

  def add(ch: Char): Unit = {
    val curr = data(ch)
    data(ch) = curr + 1
    if (curr == 0) numDistinct += 1
  }

  def remove(ch: Char): Unit = {
    val curr = data(ch)
    data(ch) = curr - 1
    if (curr == 1) numDistinct -= 1
  }
}