package com.adventofcode.twentywentytwo

import scala.io.Source
import scala.util.Using

object Day10 {

  private val registry = Registry()
  private val strengthCounter = new StrengthCounter(registry)
  private val commandParser = new CommandParser(registry, strengthCounter)

  def main(args: Array[String]): Unit = {

    Using(Source.fromResource("inputs/day10.txt")) { source => source.getLines().foreach { line =>
      commandParser.parse(line)
    }}

    println(strengthCounter.getTotal)
  }
}

private final case class Registry(var value: Long = 1)

private final class StrengthCounter(registry: Registry) {
  private var total: Long = 0
  private var cycle: Int = 0

  def tick(): Unit = {
    cycle += 1
    if (cycle <= 220 && (cycle - 20) % 40 == 0) total += (cycle * registry.value)
  }

  def getTotal: Long = total
}

private final class CommandParser(registry: Registry, strengthCounter: StrengthCounter) {
  def parse(line: String): Unit = {
    val command = line.trim.toLowerCase
    if (command == "noop")
      strengthCounter.tick()

    if (command.startsWith("addx")) {
      val amt: Int = command.split(" ")(1).toInt
      strengthCounter.tick()
      strengthCounter.tick()
      registry.value += amt
    }
  }
}