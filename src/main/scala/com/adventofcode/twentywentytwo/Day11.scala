package com.adventofcode.twentywentytwo

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.io.Source
import scala.util.Using

object Day11 {
  def main(args: Array[String]): Unit = {
    parseAndPlay(20, divideByThree)
    println("----------------------------------")
    parseAndPlay(10000)
  }

  private def divideByThree(num: Long): Long = num / 3

  private def parseAndPlay(rounds: Int, manageAnxiety: Long => Long = in => in): Unit = {
    Using(Source.fromResource("inputs/day11.txt")) { source =>
      val monkeys = Monkeys.parse(source.getLines())
      playAllRounds(monkeys, rounds, manageAnxiety)
    }
  }

  private def playAllRounds(monkeys: Seq[Monkey], rounds: Int, manageAnxiety: Long => Long): Unit = {
    val totalFactors = monkeys.map(_.divisibilityFactor).product
    (0 until rounds).foreach(_ => playRound(monkeys, manageAnxiety, totalFactors))
    printMonkeyItems(monkeys)
  }

  private def playRound(monkeys: Seq[Monkey], manageAnxiety: Long => Long, totalFactors: Long): Unit = {
    monkeys.foreach { currentMonkey =>
      while (currentMonkey.items.nonEmpty) {
        val item = currentMonkey.items.remove(0)
        currentMonkey.inspectedItems += 1
        val worryLevel: Long = manageAnxiety(currentMonkey.operation(item)) % totalFactors
        val testResult: Boolean = worryLevel % currentMonkey.divisibilityFactor == 0
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
      .sorted(Ordering[Long].reverse)
      .take(2)
      .product
    println(s"Monkey business level: $businessLevel")
  }
}

private final class Monkey(
  var items: mutable.ListBuffer[Long],
  var operation: Long => Long,
  var divisibilityFactor: Long,
  var destinations: Map[Boolean, Int],
) {
  var inspectedItems: Long = 0
}

private object Monkeys {
  def parse(input: Iterator[String]): Seq[Monkey] = {
    val buf = ListBuffer[Monkey]()
    while (input.hasNext) {
      input.next()
      buf.addOne(new Monkey(
        items = input.next().items,
        operation = input.next().operation,
        divisibilityFactor = input.next().divisibilityFactor,
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
    private val operationRegexp = """(old|\d+)\s([*+])\s(old|\d+)""".r

    def items: ListBuffer[Long] =
      ListBuffer(line.substring(line.indexOf(": ") + 2).split(",").map(_.trim.toLong): _*)

    def operation: Long => Long = {
      val operationStr = line.substring(line.indexOf("=") + 2).trim
      operationStr match {
        case operationRegexp(left, op, right) =>
          v =>
            val op1 = if (left == "old") v else left.toLong
            val op2 = if (right == "old") v else right.toLong
            if (op == "*") op1 * op2 else op1 + op2
      }
    }

    def divisibilityFactor: Long = line.substring(line.indexOf("by ") + 3).toLong

    def destination: Int = line.substring(line.indexOf("monkey") + 7).toInt
  }
}