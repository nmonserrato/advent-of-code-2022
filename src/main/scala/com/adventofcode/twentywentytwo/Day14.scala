package com.adventofcode.twentywentytwo

import java.lang.Math.{max, min}
import scala.io.Source
import scala.util.Using

object Day14 {
  def main(args: Array[String]): Unit = {
    val rocks = parseRocks
    val grid = new RockGrid(rocks)
    println(grid.maxGrainsDroppingFrom(500))
    println(grid)
  }

  private def parseRocks: Seq[RockPath] = {
    Using(Source.fromResource("inputs/day14.txt")) { source =>
      source.getLines().map { line =>
        RockPath(line.split("->").map(RockPoint.from))
      }.toSeq
    }.get
  }
}

private final case class RockPath(points: Seq[RockPoint]) {
  override def toString: String = points.map(_.toString).mkString(" => ")
}

private case class RockWallBounds(leftMost: Int, rightMost: Int, downMost: Int)

private case class RockPoint(x: Int, y: Int) {
  override def toString: String = s"[$x,$y]"
}

private object RockPoint {
  def from(str: String): RockPoint = {
    val coords = str.trim.split(",")
    RockPoint(coords.head.toInt, coords(1).toInt)
  }
}

private class RockGrid(rocks: Seq[RockPath]) {
  private[this] val bounds = findBounds
  private[this] val (rows, cols) = (bounds.downMost + 1, bounds.rightMost - bounds.leftMost + 3)
  private[this] val data = new Array[Array[Char]](rows)

  data.indices.foreach { r =>
    data(r) = new Array[Char](cols)
    (0 until cols).foreach { c => data(r)(c) = '.' }
  }

  rocks.foreach { path =>
    (0 until path.points.size - 1).foreach { idx =>
      fillLine(path.points(idx), path.points(idx + 1))
    }
  }

  def maxGrainsDroppingFrom(x: Int): Int = {
    var dropped = 0
    while (!dropSand(x)) dropped+=1
    dropped
  }

  /**
   * @param x from which x the sand falls
   * @return true if the sand overflew
   */
  private[this] def dropSand(x: Int): Boolean = {
    var y = 0
    var col = normalized(x)
    data(y)(col) = 'o'

    def moveIfPossible: Int = {
      if (y >= rows - 1) return -1 // game over!
      if (data(y + 1)(col) == '.') {
        data(y)(col) = '.'
        data(y + 1)(col) = 'o'
        y += 1
        return 0 // air right down
      }
      if (data(y + 1)(col - 1) == '.') {
        data(y)(col) = '.'
        data(y + 1)(col - 1) = 'o'
        y += 1
        col -= 1
        return 0 // air down left
      }
      if (data(y + 1)(col + 1) == '.') {
        data(y)(col) = '.'
        data(y + 1)(col + 1) = 'o'
        y += 1
        col += 1
        return 0 // air down left
      }

      1 // cannot move any further
    }

    var last = 0
    while (last == 0) last = moveIfPossible
    last == -1
  }

  private[this] def fillLine(from: RockPoint, until: RockPoint): Unit = {
    val (fx, tx) = if (from.x < until.x) (from.x, until.x) else (until.x, from.x)
    val (fy, ty) = if (from.y < until.y) (from.y, until.y) else (until.y, from.y)
    (fx to tx).map { x =>
      (fy to ty).map { y =>
        data(y)(normalized(x)) = '#'
      }
    }
  }

  private[this] def normalized(x: Int): Int = x - bounds.leftMost + 1

  override def toString: String = data.map(row => row.mkString).mkString("\n")

  private[this] def findBounds: RockWallBounds = {
    val (lm, rm, dm) = rocks
      .map { path =>
        val lm = path.points.map(_.x).min
        val rm = path.points.map(_.x).max
        val bm = path.points.map(_.y).max
        (lm, rm, bm)
      }
      .reduce((q1, q2) => (min(q1._1, q2._1), max(q1._2, q2._2), max(q1._3, q2._3)))

    RockWallBounds(lm, rm, dm)
  }
}