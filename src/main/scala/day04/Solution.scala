package day04

import scala.io.Source
import scala.util.Using

object Solution {
  def main(args: Array[String]): Unit = {
    var overlapsCompletely = 0
    var overlapsPartially = 0

    Using(Source.fromResource("day04/input.txt")) { source =>
      source.getLines().foreach { line =>
        val (r1, r2) = Ranges.parse(line)
        if (r1.containsEntirely(r2) || r2.containsEntirely(r1)) overlapsCompletely += 1
        if (r1.overlapsWith(r2)) overlapsPartially += 1
      }
    }

    println(s"overlapsCompletely=$overlapsCompletely")
    println(s"overlapsPartially=$overlapsPartially")
  }
}

final case class Range(start: Int, end: Int) {
  def containsEntirely(other: Range): Boolean = {
    (this.start <= other.start && this.end >= other.end) ||
      (this.start >= other.start && this.end <= other.end)
  }

  def overlapsWith(other: Range): Boolean = {
    this.start <= other.end && other.start <= this.end
  }
}

object Range {
  def parse(str: String): Range = {
    val nums = str.split("-").map(_.toInt)
    new Range(nums.head, nums(1))
  }
}
object Ranges {
  def parse(str: String): (Range, Range)  = {
    val rs = str.split(",")
    (Range.parse(rs.head), Range.parse(rs(1)))
  }
}