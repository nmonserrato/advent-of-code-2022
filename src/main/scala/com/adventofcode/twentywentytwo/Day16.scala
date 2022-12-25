package com.adventofcode.twentywentytwo

import java.util.concurrent.ConcurrentHashMap
import scala.collection.mutable
import scala.io.Source
import scala.jdk.CollectionConverters.ConcurrentMapHasAsScala
import scala.util.Try
import scala.util.Using

object Day16 {
  private val valves: mutable.HashMap[String, Int] = new mutable.HashMap()
  private val tunnels: mutable.HashMap[String, Seq[String]] = new mutable.HashMap()
  parseInput
  private val usefulValves: Set[String] = valves.filter(_._2 > 0).keySet.toSet
  private val memo = new ConcurrentHashMap[String, Int](100000, 2).asScala

  def main(args: Array[String]): Unit = {
    val start = System.currentTimeMillis()
    println(mostPressure("AA", false, usefulValves, 30))
    println(s"Elapsed: ${System.currentTimeMillis() - start} ms")
    memo.clear()
    println(mostPressure("AA", true, usefulValves, 26))
    println(s"Elapsed: ${System.currentTimeMillis() - start} ms")
  }

  private def mostPressure(current: String, hasHelper: Boolean, closedValves: Set[String], minutesLeft: Int): Int = {
    if (minutesLeft <= 1 && hasHelper && closedValves.nonEmpty)
      return mostPressure("AA", false, closedValves, 26)

    if (minutesLeft <= 1 || closedValves.isEmpty) return 0

    val memoKey = s"$current-$hasHelper-${closedValves.toSeq.sorted.mkString("_")}-$minutesLeft"
    if (memo.contains(memoKey)) return memo(memoKey)

    val mostPressureOpening =
      if (closedValves.contains(current))
        (minutesLeft - 1) * valves(current) + mostPressure(current, hasHelper, closedValves - current, minutesLeft - 1)
      else 0
    val mostPressureMoving = tunnels(current)
      .map(nextRoom => mostPressure(nextRoom, hasHelper, closedValves, minutesLeft - 1))
      .max

    val mostPressureFromHere = Math.max(mostPressureOpening, mostPressureMoving)
    memo(memoKey) = mostPressureFromHere
    mostPressureFromHere
  }

  private def parseInput: Try[Unit] = {
    val inputRegexp = "Valve (.+) has flow rate=(\\d+); tunnels? leads? to valves? (.+)".r
    Using(Source.fromResource("inputs/day16.txt")) { source =>
      source.getLines().foreach {
        case inputRegexp(name, flowRateStr, tunnelsStr) =>
          val flowRate = flowRateStr.toInt
          valves(name) = flowRate
          tunnels(name) = tunnelsStr.split(",").map(_.trim)
      }
    }
  }
}
