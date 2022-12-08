package com.adventofcode.twentywentytwo

import scala.io.Source
import scala.util.Using

object Day04 {
  def main(args: Array[String]): Unit = {
    var overlapsCompletely = 0
    var overlapsPartially = 0

    Using(Source.fromResource("inputs/day04.txt")) { source =>
      source.getLines().foreach { line =>
        val (r1, r2) = Intervals.parse(line)
        if (r1.containsEntirely(r2) || r2.containsEntirely(r1)) overlapsCompletely += 1
        if (r1.overlapsWith(r2)) overlapsPartially += 1
      }
    }

    println(s"overlapsCompletely=$overlapsCompletely")
    println(s"overlapsPartially=$overlapsPartially")
  }
}

final case class Interval(start: Int, end: Int) {
  def containsEntirely(other: Interval): Boolean = {
    (this.start <= other.start && this.end >= other.end) ||
      (this.start >= other.start && this.end <= other.end)
  }

  def overlapsWith(other: Interval): Boolean = {
    this.start <= other.end && other.start <= this.end
  }
}

object Interval {
  def parse(str: String): Interval = {
    val nums = str.split("-").map(_.toInt)
    new Interval(nums.head, nums(1))
  }
}
object Intervals {
  def parse(str: String): (Interval, Interval)  = {
    val rs = str.split(",")
    (Interval.parse(rs.head), Interval.parse(rs(1)))
  }
}