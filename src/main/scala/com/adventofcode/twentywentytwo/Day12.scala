package com.adventofcode.twentywentytwo

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.io.Source
import scala.util.Using

object Day12 {
  def main(args: Array[String]): Unit = {
    val grid = Grid.parseFromFile
    println(grid.distanceFromStart)
    println(grid.minDistanceFromAll('a'))
  }
}

private class Grid(data: Array[Array[Char]], start: Point, end: Point) {
  private[this] val rows = data.length
  private[this] val cols = data.head.length

  def distanceFromStart: Int = distanceFromPoint(start)

  def minDistanceFromAll(startChar: Char): Int = {
    (0 until rows).flatMap { row =>
      (0 until cols).filter(data(row)(_) == startChar).map { col =>
        distanceFromPoint(Point(row, col))
      }
    }.filter(_ > 0).min
  }

  private def distanceFromPoint(startPoint: Point): Int = {
    val distances = new mutable.HashMap[Point, Int]()
    val queue = new mutable.Queue[Point]()
    val updatedLastIteration = new mutable.Queue[Point]()
    distances(startPoint) = 0
    updatedLastIteration.addOne(startPoint)
    while (updatedLastIteration.nonEmpty) {
      queue.addAll(updatedLastIteration)
      updatedLastIteration.clear()
      while (queue.nonEmpty) {
        val loc = queue.remove(0)
        val locDist = distances(loc)
        loc.neighbors.foreach { neighbor =>
          if (canStep(loc, neighbor) && !distances.contains(neighbor)) {
            distances(neighbor) = locDist + 1
            updatedLastIteration.addOne(neighbor)
          }
        }
      }
    }

    distances.getOrElse(end, -1)
  }

  private[this] def canStep(from: Point, to: Point): Boolean = {
    if (to.isOutOfBounds(rows, cols)) return false
    (charAt(from), charAt(to)) match {
      case ('S', toChar) => toChar == 'a'
      case ('z', toChar) => (toChar >= 'a' && toChar <= 'z') || toChar == 'E'
      case (fromChar, toChar) if fromChar >= 'a' && fromChar < 'z' => toChar <= fromChar + 1
      case _ => false
    }
  }

  private[this] def charAt(point: Point): Char = data(point.row)(point.column)
}

private case class Point(row: Int, column: Int) {
  def neighbors: Seq[Point] =
    Seq(Point(row - 1, column), Point(row, column -1), Point(row + 1, column), Point(row, column + 1))

  def isOutOfBounds(rows: Int, cols: Int): Boolean =
    row < 0 || column < 0 || row >= rows || column >= cols
}

private object Grid {
  def parseFromFile: Grid = {
    Using(Source.fromResource("inputs/day12.txt")) { source =>
      val buf = ListBuffer[Array[Char]]()
      var start: Point = null
      var end: Point = null
      source.getLines().zipWithIndex.foreach { case (line, idx) =>
        buf.addOne(line.toCharArray)
        if (line.contains('S')) start = new Point(idx, line.indexOf('S'))
        if (line.contains('E')) end = new Point(idx, line.indexOf('E'))
      }
      new Grid(buf.toArray, start, end)
    }.get
  }
}