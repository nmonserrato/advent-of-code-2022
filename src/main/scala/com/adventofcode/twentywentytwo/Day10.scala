package com.adventofcode.twentywentytwo

import scala.io.Source
import scala.util.Using

object Day10 {
  private val registry = Registry()
  private val strengthCounter = new StrengthCounter(registry)
  private val crt = new CRT(registry)
  private val commandParser = new CommandParser(registry, strengthCounter, crt)

  def main(args: Array[String]): Unit = {
    Using(Source.fromResource("inputs/day10.txt")) { source => source.getLines().foreach { line =>
      commandParser.parseAndExecute(line)
    }}

    println(strengthCounter.getTotal)
    println(crt.screen)
  }
}

private final case class Registry(
  var cycle: Int = 0,
  var value: Int = 1,
)

private final class StrengthCounter(registry: Registry) {
  private var total: Int = 0

  def evaluate(): Unit = {
    if (registry.cycle <= 220 && (registry.cycle - 20) % 40 == 0)
      total += (registry.cycle * registry.value)
  }

  def getTotal: Long = total
}

private final class CommandParser(registry: Registry, strengthCounter: StrengthCounter, crt: CRT) {
  def parseAndExecute(line: String): Unit = {
    val command = line.trim.toLowerCase
    if (command == "noop")
      tick()

    if (command.startsWith("addx")) {
      tick()
      tick()
      registry.value += command.split(" ")(1).toInt
    }
  }

  private def tick(): Unit = {
    crt.drawPixel()
    registry.cycle += 1
    strengthCounter.evaluate()
  }
}

private final class CRT(registry: Registry) {
  private val display = new Array[Char](240)

  def drawPixel(): Unit = {
    if (registry.cycle >= 240) return
    display(registry.cycle) = if (pixelPositions.contains(registry.cycle % 40)) '#' else '.'
  }

  def screen: String = {
    display.grouped(40).map(_.mkString).mkString("\n")
  }

  private def pixelPositions: Seq[Int] = Seq(registry.value - 1, registry.value, registry.value + 1)
}