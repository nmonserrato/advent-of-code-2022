package com.adventofcode.twentywentytwo

import scala.collection.mutable.ListBuffer
import scala.io.Source
import scala.util.Using

object Day15 {
  def main(args: Array[String]): Unit = {
    val allSensors = parseInput
    println(findCoveredSpotsInLine(allSensors, 2000000))
    println("Wait for tuning frequency...")
    println(findTuningFrequency(allSensors, 4000000))
  }

  private def findTuningFrequency(allSensors: Seq[SensorData], maxCoord: Int): BigInt = {
    val start = System.currentTimeMillis()

    val beaconLocation = LazyList.range(0, maxCoord).map { line =>
      val ranges = findCoveredRangesInLine(allSensors, line).bounded(0, maxCoord)
      if (rangesª.coversFully(0, maxCoord)) Coords(-1, -1)
      else Coords(ranges.firstFreeSpot, line)
    }.find(_.x >= 0).get

    println(s"Found in ${System.currentTimeMillis() - start} ms")
    BigInt(beaconLocation.x) * maxCoord + BigInt(beaconLocation.y)
  }

  private def findCoveredSpotsInLine(allSensors: Seq[SensorData], line: Int): Int = {
    val coveredSpots = findCoveredRangesInLine(allSensors, line).map(_.size).sum
    val countBeaconsOnLine: Int = allSensors.map(_.beacon).distinct.count(_.y == line)
    coveredSpots - countBeaconsOnLine
  }

  private def findCoveredRangesInLine(allSensors: Seq[SensorData], line: Int): Seq[Range] = {
    val usefulSensors = allSensors.filter {
      data => data.coords.verticalDistanceFrom(line) <= data.distanceFromBeacon
    }

    usefulSensors.map { data =>
      val distanceFromTarget = data.coords.verticalDistanceFrom(line)
      val yps = (data.distanceFromBeacon * 2 + 1 - distanceFromTarget * 2 - 1) / 2
      val sx = data.coords.x
      sx - yps to sx + yps
    }.merged
  }

  private def parseInput: Seq[SensorData] = {
    val inputRegexp = "Sensor at x=(-?[0-9]+), y=(-?[0-9]+): closest beacon is at x=(-?[0-9]+), y=(-?[0-9]+)".r
    Using(Source.fromResource("inputs/day15.txt")) { source =>
      source.getLines().map {
        case inputRegexp(sx, sy, bx, by) =>
          val beacon = Coords(bx.toInt, by.toInt)
          val coords = Coords(sx.toInt, sy.toInt)
          SensorData(coords, beacon)
      }.toSeq
    }.get
  }

  private implicit class RangesMerger(ranges: Seq[Range]) {
    def merged: Seq[Range] = {
      val sorted = ranges.sortBy(_.start)
      val acc = new ListBuffer[Range]()
      acc.addOne(sorted.head)
      sorted.drop(1).foreach { next =>
        if (next.start <= acc.last.end) acc(acc.length - 1) = acc.last.start to Math.max(acc.last.end, next.end)
        else acc.addOne(next)
      }
      acc.toSeq
    }

    def bounded(from: Int, until: Int): Seq[Range] =
      ranges.map(r => Range.inclusive(Math.max(from, r.start), Math.min(r.end, until)))

    def coversFully(from: Int, until: Int): Boolean = ranges == Seq(Range.inclusive(from, until))

    def firstFreeSpot: Int = ranges.head.end + 1
  }
}

private final case class SensorData(coords: Coords, beacon: Coords) {
  val distanceFromBeacon: Int = coords.distanceFrom(beacon)
}

private final case class Coords(x: Int, y: Int) {
  def distanceFrom(other: Coords): Int = Math.abs(this.x - other.x) + Math.abs(this.y - other.y)
  def verticalDistanceFrom(lineNumber: Int): Int = Math.abs(this.y - lineNumber)
}
