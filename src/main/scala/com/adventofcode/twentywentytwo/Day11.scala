package com.adventofcode.twentywentytwo

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.io.Source
import scala.util.Using

object Day11 {
  def main(args: Array[String]): Unit = {
    Using(Source.fromResource("inputs/day11.txt")) { source =>
      val monkeys = Monkeys.parse(source.getLines())
      playAllRounds(monkeys, 20, 3)
    }
  }

  private def playAllRounds(monkeys: Seq[Monkey], rounds: Int, divider: Int): Unit = {
    (0 until rounds).foreach(_ => playRound(monkeys, divider))
    printMonkeyItems(monkeys)
  }

  private def playRound(monkeys: Seq[Monkey], divider: Int): Unit = {
    monkeys.foreach { currentMonkey =>
      while (currentMonkey.items.nonEmpty) {
        val item = currentMonkey.items.remove(0)
        currentMonkey.inspectedItems += 1
        val worryLevel: Int = currentMonkey.operation(item) / divider
        val testResult: Boolean = currentMonkey.test(worryLevel)
        monkeys(currentMonkey.destinations(testResult)).items.addOne(worryLevel)
      }
    }
  }

  private def printMonkeyItems(monkeys: Seq[Monkey]): Unit = {
    monkeys.zipWithIndex.foreach { case (m, idx) => println(s"Monkey $idx: ${m.items.mkString(", ")}") }
    println()
    monkeys.zipWithIndex.foreach { case (m, idx) => println(s"Monkey $idx inspected items ${m.inspectedItems} times") }
    println()
    val businessLevel = monkeys.map(_.inspectedItems)
      .sorted(Ordering[Int].reverse)
      .take(2)
      .product
    println(s"Monkey business level $businessLevel")
  }
}

private final class Monkey(
  var items: mutable.ListBuffer[Int],
  var operation: Int => Int,
  var test: Int => Boolean,
  var destinations: Map[Boolean, Int],
) {
  var inspectedItems = 0
}

private object Monkeys {
  def parse(input: Iterator[String]): Seq[Monkey] = {
    val buf = ListBuffer[Monkey]()
    while (input.hasNext) {
      input.next()
      buf.addOne(new Monkey(
        items = input.next().items,
        operation = input.next().operation,
        test = input.next().test,
        destinations = Seq(
          true -> input.next().destination,
          false -> input.next().destination,
        ).toMap
      ))
      if (input.hasNext) input.next()
    }
    buf.toSeq
  }

  implicit class MonkeyLines(line: String) {
    private val operationRegexp = """(old|\d+)\s(\*|\+)\s(old|\d+)""".r

    def items: ListBuffer[Int] =
      ListBuffer(line.substring(line.indexOf(": ") + 2).split(",").map(_.trim.toInt): _*)

    def operation: Int => Int = {
      val operationStr = line.substring(line.indexOf("=") + 2).trim
      operationStr match {
        case operationRegexp(left, op, right) =>
          v =>
            val op1 = if (left == "old") v else left.toInt
            val op2 = if (right == "old") v else right.toInt
            if (op == "*") op1 * op2 else op1 + op2
        case _ => throw new IllegalArgumentException(s"Invalid pattern $operationStr")
      }
    }

    def test: Int => Boolean = {
      val num: Int = line.substring(line.indexOf("by ") + 3).toInt
      input: Int => (input % num) == 0
    }

    def destination: Int = line.substring(line.indexOf("monkey") + 7).toInt

    private def plus(op1: Int)(op2: Int): Int = op1 + op2
    private def multiply(op1: Int)(op2: Int): Int = op1 * op2
  }
}