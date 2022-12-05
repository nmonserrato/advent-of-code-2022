package com.adventofcode.twentywentytwo

import scala.collection.mutable
import scala.io.Source
import scala.util.Using

object Solution {
  def main(args: Array[String]): Unit = {
    println(groupAndFindTop().total)
    println(groupAndFindTop(3).total)
  }

  private def groupAndFindTop(howMany: Int = 1): MaxHeap = {
    var currTotal: Long = 0
    val maxHeap = new MaxHeap(howMany)
    Using(Source.fromResource("inputs/day01.txt")) { source =>
      source.getLines().foreach { line =>
        if (line.isBlank) {
          maxHeap += currTotal
          currTotal = 0
        } else {
          currTotal += line.toLong
        }
      }
    }.get
    maxHeap
  }
}

class MaxHeap(size: Int) {
  private val data = new mutable.PriorityQueue[Long]()(Ordering[Long].reverse)
  def +=(elem : Long): Unit = {
    data += elem
    if (data.size > size) data.dequeue()
  }

  def total: Long = data.sum
}