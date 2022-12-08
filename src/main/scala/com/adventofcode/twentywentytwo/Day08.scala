package com.adventofcode.twentywentytwo

import scala.collection.mutable
import scala.io.Source
import scala.util.Using

object Day08 {

  def main(args: Array[String]): Unit = {
    Using(Source.fromResource("inputs/day08.txt")) { source =>
      val grid = parseGrid(source.getLines())
      println(findVisible(grid))
      println(findBestScenicTree(grid))
    }
  }

  private def parseGrid(iterator: Iterator[String]): Array[Array[Byte]] = {
    iterator.map(line => line.map(_.toByte).toArray).toArray
  }

  private def calculateScenicScoreFor(grid: Array[Array[Byte]], r: Int, c: Int): Int = {
    val (rows, cols) = (grid.length, grid(0).length)

    if (r == 0 || c == 0 || r == rows -1 || c == cols - 1) return 0

    var (li, ri, ui, bi) = (c - 1, c + 1, r - 1, r + 1)
    var (lc, rc, uc, bc) = (0, 0, 0, 0)
    while (li > 0 && grid(r)(li) < grid(r)(c)) { lc += 1; li -= 1 }
    while (ri < cols-1 && grid(r)(ri) < grid(r)(c)) { rc += 1; ri += 1 }
    while (ui > 0 && grid(ui)(c) < grid(r)(c)) { uc += 1; ui -= 1 }
    while (bi < cols-1 && grid(bi)(c) < grid(r)(c)) { bc += 1; bi += 1 }
    (lc+1) * (rc+1) * (uc+1) * (bc+1)
  }

  private def findBestScenicTree(array: Array[Array[Byte]]): Long = {
    var bestScore = -1
    array.indices.foreach(r =>
      array(0).indices.foreach(c =>
        bestScore = Math.max(bestScore, calculateScenicScoreFor(array, r, c))
      )
    )
    bestScore
  }

  private def findVisible(grid: Array[Array[Byte]]): Int = {
    val (rows, cols) = (grid.length, grid(0).length)

    (findVisible(grid, 0 until rows, 0 until cols, true) ++
    findVisible(grid, 0 until rows, cols - 1 to 0 by -1, true) ++
    findVisible(grid, 0 until cols, 0 until rows, false) ++
    findVisible(grid, 0 until cols, rows - 1 to 0 by -1, false)).size
  }

  private def findVisible(grid: Array[Array[Byte]], r1: Range, r2: Range, r1IsRow: Boolean): mutable.Set[(Int, Int)] = {
    val visible = mutable.Set[(Int, Int)]()
    r1.foreach { i1 =>
      var maxSoFar = -1
      r2.foreach { i2 =>
        val (r, c) = if (r1IsRow) (i1, i2) else (i2, i1)
        if (grid(r)(c) > maxSoFar) {
          visible.addOne((r, c))
          maxSoFar = grid(r)(c)
        }
      }
    }
    visible
  }
}