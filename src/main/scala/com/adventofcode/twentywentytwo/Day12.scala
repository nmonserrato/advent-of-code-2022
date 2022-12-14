package com.adventofcode.twentywentytwo

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.io.Source
import scala.util.Using

object Day12 {
  def main(args: Array[String]): Unit = {
    val (grid, start, end) = parseGrid()
    println(findDistance(grid, start, end))

    val bestFromAs = grid.indices.flatMap(row =>
      grid(row).indices.filter(grid(row)(_) == 'a').map(col =>
        findDistance(grid, (row, col), end)
      )
    ).filter(_ > 0).min

    println(bestFromAs)
  }

  private def findDistance(grid: Array[Array[Char]], start: (Int, Int), end: (Int, Int)): Int = {
    val distances = new Array[Array[Int]](grid.size)
    grid.indices.foreach(row => distances(row) = new Array[Int](grid.head.size))
    grid.indices.foreach(r => distances(r).indices.foreach(c => distances(r)(c) = -1))
    val queue = new mutable.Queue[(Int, Int)]()
    val updatedLastIteration = new mutable.Queue[(Int, Int)]()
    distances(start._1)(start._2) = 0
    updatedLastIteration.addOne(start)
    while (updatedLastIteration.nonEmpty) {
      queue.addAll(updatedLastIteration)
      updatedLastIteration.clear()
      while (queue.nonEmpty) {
        val loc = queue.remove(0)
        val locDist = distances(loc._1)(loc._2)
        Seq((-1, 0), (0, -1), (1, 0), (0, 1)).foreach { mov =>
          val neighbor = (loc._1 + mov._1, loc._2 + mov._2)
          if (canStep(grid, loc, neighbor) && (distances(neighbor._1)(neighbor._2) == -1)) {
            distances(neighbor._1)(neighbor._2) = locDist + 1
            updatedLastIteration.addOne(neighbor)
          }
        }
      }
    }

    distances(end._1)(end._2)
  }

  private def canStep(grid: Array[Array[Char]], from: (Int, Int), to: (Int, Int)): Boolean = {
    if (to._1 < 0 || to._2 < 0 || to._1 >= grid.length || to._2 >= grid.head.length) return false
    val fromChar = grid(from._1)(from._2)
    val toChar = grid(to._1)(to._2)
    if (fromChar == 'S') return toChar == 'a'
    if (fromChar >= 'a' && fromChar < 'z') return toChar <= fromChar + 1
    if (fromChar == 'z') return (toChar >='a' && toChar <= 'z') || toChar == 'E'
    false
  }

  private def parseGrid(): (Array[Array[Char]], (Int, Int), (Int, Int)) = {
    Using(Source.fromResource("inputs/day12.txt")) { source =>
      val buf = ListBuffer[Array[Char]]()
      var start = (-1, -1)
      var end = (-1, -1)
      source.getLines().zipWithIndex.foreach { case (line, idx) =>
        buf.addOne(line.toCharArray)
        if (line.contains('S')) start = (idx, line.indexOf('S'))
        if (line.contains('E')) end = (idx, line.indexOf('E'))
      }
      (buf.toArray, start, end)
    }.get
  }
}
