package com.adventofcode.twentywentytwo

import java.lang.Math.{max, min}
import scala.io.Source
import scala.util.Using

object Day14 {
  def main(args: Array[String]): Unit = {
    val rocks = parseRocks
    val grid = new RockGrid(rocks)
    println(grid.maxGrainsTillOverflow(500))
    println(grid.maxGrainsTillFull(500))
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
  private[this] val rows = bounds.downMost + 3
  private[this] val extraColsPerSide = rows + 1
  private[this] val cols = bounds.rightMost - bounds.leftMost + extraColsPerSide * 2 + 1
  private[this] val data = new Array[Array[Char]](rows)
  private[this] var grainsDropped = 0
  initializeGrid()

  override def toString: String = data.map(row => row.mkString).mkString("\n")

  def maxGrainsTillOverflow(x: Int): Int = {
    dropSandWhile(x)(_ < rows - 2)
    grainsDropped - 1
  }

  def maxGrainsTillFull(x: Int): Int = {
    dropSandWhile(x)(_ > 0)
    grainsDropped - 2
  }

  private[this] def dropSandWhile(x: Int)(test: Int => Boolean): Unit = {
    var restY = -1
    do {
      restY = dropSand(x)
    } while (test(restY))
  }

  /**
   * @param x from which x the sand falls
   * @return the last y where the grain came to rest
   */
  private[this] def dropSand(x: Int): Int = {
    var y = 0
    var col = normalized(x)
    data(y)(col) = 'o'
    grainsDropped += 1

    def moveTo(dest: RockPoint): Boolean = {
      if (data(dest.y)(dest.x) == '.') {
        data(y)(col) = '.'
        data(dest.y)(dest.x) = 'o'
        y = dest.y
        col = dest.x
        return true
      }
      false
    }

    def moveIfPossible: Boolean =
      moveTo(RockPoint(col, y + 1)) ||
        moveTo(RockPoint(col - 1, y + 1)) ||
        moveTo(RockPoint(col + 1, y + 1))

    var moved = true
    while (moved) moved = moveIfPossible
    y
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

  private[this] def normalized(x: Int): Int = x - bounds.leftMost + extraColsPerSide

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

  private[this] def initializeGrid(): Unit = {
    data.indices.foreach { r =>
      data(r) = new Array[Char](cols)
      (0 until cols).foreach { c => data(r)(c) = '.' }
    }

    (0 until cols).foreach { x => data.last(x) = '#' }

    rocks.foreach { path =>
      (0 until path.points.size - 1).foreach { idx =>
        fillLine(path.points(idx), path.points(idx + 1))
      }
    }
  }
}
