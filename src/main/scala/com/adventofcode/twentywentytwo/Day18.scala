package com.adventofcode.twentywentytwo

import scala.collection.mutable
import scala.io.Source
import scala.util.Using

object Day18 {
  private val points: Seq[Point3D] = parseInput
  private val existing = new mutable.HashSet[Point3D](points.size, 2.0)

  def main(args: Array[String]): Unit = {
    var countFaces = 0L
    for (p <- points) {
      countFaces += 6
      p.neighbors.foreach(n => if (existing.contains(n)) countFaces -= 2)
      existing.add(p)
    }
    println(countFaces)
  }

  private def parseInput: Seq[Point3D] = {
    Using(Source.fromResource("inputs/day18.txt")) { source =>
      source.getLines().map(Point3D.from).toSeq
    }.get
  }
}

private final case class Point3D(x: Int, y: Int, z: Int) {
  def neighbors: Seq[Point3D] =
    Seq((1, 0, 0), (-1, 0, 0), (0, 1, 0), (0, -1, 0), (0, 0, 1), (0, 0, -1)) // the 6 faces
      .map(diff => Point3D(this.x + diff._1, this.y + diff._2, this.z + diff._3))
}

private object Point3D {
  def from(str: String): Point3D = {
    val parts = str.trim.split(",").map(_.trim.toInt)
    Point3D(parts.head, parts(1), parts(2))
  }
}
