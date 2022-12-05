package com.adventofcode.twentywentytwo

import scala.io.Source
import scala.util.Using

object Day02 {
  def main(args: Array[String]): Unit = {

    Using(Source.fromResource("inputs/day02.txt")) { source =>
      val total = source.getLines().map { line =>
        val spl = line.split(" ")
        val chosenShape = shapeForRound(s"${spl(1)}_${spl.head}")
        scoreForShape(chosenShape) + scoreForRound(spl(1))
      }.sum

      println(s"$total")
    }
  }

  private val scoreForShape = Map[String, Int](
    "B" -> 2,
    "A" -> 1,
    "C" -> 3,
  )

  private val shapeForRound = Map[String, String](elems =
    "X_A" -> "C",
    "X_B" -> "A",
    "X_C" -> "B",
    "Y_A" -> "A",
    "Y_B" -> "B",
    "Y_C" -> "C",
    "Z_A" -> "B",
    "Z_B" -> "C",
    "Z_C" -> "A",
  )

  private val scoreForRound = Map[String, Int](
    "X" -> 0,
    "Y" -> 3,
    "Z" -> 6,
  )
}
