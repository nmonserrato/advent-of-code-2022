package com.adventofcode.twentywentytwo

import com.adventofcode.twentywentytwo.XY.{STEP_DOWN, STEP_LEFT, STEP_RIGHT, STEP_UP}

import scala.collection.mutable
import scala.io.Source
import scala.util.Using

object Day09 {
  private var h = XY.INITIAL
  private var t = XY.INITIAL
  private val visited = mutable.HashSet[XY]()

  def main(args: Array[String]): Unit = {
    Using(Source.fromResource("inputs/day09.txt")) { source => source.getLines().foreach { line =>
      val (step, howMany) = parseSteps(line)
      (0 until howMany).foreach(_ => makeMove(step))
    }}

    println(visited.size)
  }

  private def makeMove(step: XY): Unit = {
    h = h + step
    if (t doesntNeedToMoveToward h) return
    h diff t match {
      case (x, y) =>
        if (x > 0) t = t + STEP_RIGHT
        if (x < 0) t = t + STEP_LEFT
        if (y > 0) t = t + STEP_DOWN
        if (y < 0) t = t + STEP_UP
    }
    visited.add(t)
  }

  private def parseSteps(line: String): (XY, Int) = {
    val split = line.split(" ")
    split(0) match {
      case "R" => (STEP_RIGHT, split(1).toInt)
      case "L" => (STEP_LEFT, split(1).toInt)
      case "U" => (STEP_UP, split(1).toInt)
      case "D" => (STEP_DOWN, split(1).toInt)
      case x => throw new IllegalArgumentException(s"Invalid move $x")
    }
  }
}

private final case class XY(x: Int, y: Int) {
  def +(other: XY): XY = XY(this.x + other.x, this.y + other.y)

  def diff(other: XY): (Int, Int) = (this.x - other.x, this.y - other.y)

  def doesntNeedToMoveToward(other: XY): Boolean = (this.x - other.x).abs <= 1 && (this.y - other.y).abs <= 1
}

private object XY {
  final val INITIAL = XY(0, 0)
  final val STEP_RIGHT = XY(1, 0)
  final val STEP_LEFT = XY(-1, 0)
  final val STEP_UP = XY(0, -1)
  final val STEP_DOWN = XY(0, 1)
}