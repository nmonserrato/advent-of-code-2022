package day05

import scala.collection.mutable
import scala.io.Source
import scala.util.Using

object Solution {

  def main(args: Array[String]): Unit = {
    moveCrates(OneByOne)
    moveCrates(AsBlock)
  }

  private def moveCrates(movingStrategy: MovingStrategy): Unit = {
    Using(Source.fromResource("day05/input.txt")) { source =>
      val iterator = source.getLines()
      val stacks = readInitialState(iterator)
      while (iterator.hasNext) executeMove(stacks, iterator.next(), movingStrategy)
      val output = topOfEach(stacks).mkString
      println(output)
    }
  }

  private def topOfEach(stacks: Array[mutable.Stack[Char]]): Array[Char] = stacks.flatMap(_.headOption)

  private def executeMove(stacks: Array[mutable.Stack[Char]], line: String, movingStrategy: MovingStrategy): Unit = {
    val regexp = "move ([0-9]+) from ([1-9]) to ([0-9])".r
    regexp.findFirstMatchIn(line).foreach { patternMatch =>
      val (howMany, from, to) = (patternMatch.group(1).toInt, patternMatch.group(2).toInt, patternMatch.group(3).toInt)
      movingStrategy.execute(stacks, new Move(howMany, from, to))
    }
  }

  private def readInitialState(lines: Iterator[String]): Array[mutable.Stack[Char]] = {
    var line = lines.next()
    val numOfStacks = (line.length + 1 ) / 4

    val tmp = emptyStacks(numOfStacks)
    while (!line.isBlank) {
      if (line.trim.startsWith("[")) {
        pushInto(tmp, line)
      }
      line = lines.next()
    }

    val out = emptyStacks(numOfStacks)
    tmp.zipWithIndex.foreach { case (stack, idx) =>
      while (stack.nonEmpty) out(idx).push(stack.pop())
    }
    out
  }

  private def emptyStacks(numOfStacks: Int) = {
    val stacks = new Array[mutable.Stack[Char]](numOfStacks)
    (0 until numOfStacks).foreach{ i => stacks(i) = new mutable.Stack[Char]() }
    stacks
  }

  private def pushInto(stack: Array[mutable.Stack[Char]], line: String): Unit = {
    line.grouped(4).zipWithIndex.foreach { case(str, idx) =>
      if (str.startsWith("[")) stack(idx).push(str.charAt(1))
    }
  }
}

private case class Move(howMany: Int, from: Int, to: Int)

private sealed trait MovingStrategy {
  def execute(stack: Array[mutable.Stack[Char]], move: Move): Unit
}

private object OneByOne extends MovingStrategy {
  override def execute(stacks: Array[mutable.Stack[Char]], move: Move): Unit =
    (1 to move.howMany).foreach { _ => stacks(move.to - 1).push(stacks(move.from - 1).pop()) }
}

private object AsBlock extends MovingStrategy {
  override def execute(stacks: Array[mutable.Stack[Char]], move: Move): Unit = {
    val buf = new mutable.Stack[Char](move.howMany)
    (1 to move.howMany).foreach { _ => buf.push(stacks(move.from - 1).pop()) }
    while (buf.nonEmpty) stacks(move.to - 1).push(buf.pop())
  }
}

