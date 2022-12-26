package com.adventofcode.twentywentytwo

import scala.collection.mutable
import scala.io.Source
import scala.util.Using

object Day17 {
  private val jetPattern: String = parseInput
  private val howManyRocks = 1000000000000L // <-- change this to 2022 for part 1
  private val shapes = Array(Shapes.horizonalLine, Shapes.cross, Shapes.l, Shapes.verticalLine, Shapes.square)
  private val tower = new Tower(jetPattern.size * 3)
  private var nextJetIndex = 0
  private var currentShape: Shape = shapes(0)
  private val detectedShape0Positions = new mutable.HashMap[Int, (Long, Int)]()
  private var simulatedHeight = -1L

  def main(args: Array[String]): Unit = {
    var nextShapeIndex = 0L
    while (nextShapeIndex < howManyRocks) {
      val shapeIndex = nextShapeIndex % shapes.length
      if (shapeIndex == 0 && simulatedHeight < 0) {
        val jetIndex = nextJetIndex%jetPattern.length
        if (detectedShape0Positions.contains(jetIndex)) {
          val heightPerIteration = tower.height - detectedShape0Positions(jetIndex)._2
          val shapesPerIteration = nextShapeIndex - detectedShape0Positions(jetIndex)._1
          val remainingShapes = howManyRocks - nextShapeIndex
          val simulatedIterations = remainingShapes/shapesPerIteration
          val simulatedShapes = simulatedIterations * shapesPerIteration
          val extraShapes = (remainingShapes - simulatedIterations * shapesPerIteration).toInt
          println(s"Each iteration adds $shapesPerIteration shapes and $heightPerIteration in height. " +
            s"Remaining $remainingShapes shapes. Will simulate $simulatedShapes and play $extraShapes more.")
          simulatedHeight = simulatedIterations * heightPerIteration
          nextShapeIndex = howManyRocks - extraShapes
        } else {
          detectedShape0Positions(jetIndex) = (nextShapeIndex, tower.height)
        }
      }
      throwRock(nextShapeIndex)
      nextShapeIndex += 1
    }

    simulatedHeight = Math.max(simulatedHeight, 0)
    println(simulatedHeight + tower.height)
  }

  private def throwRock(shapeIndex: Long): Unit = {
    currentShape = shapes((shapeIndex % shapes.length).toInt)
    var pos = tower.addNew(currentShape)
    var movedDown = true

    while (movedDown) {
      if (jetPattern(nextJetIndex) == '>') pos = tower.moveRight(currentShape, pos)
      else pos = tower.moveLeft(currentShape, pos)
      val lastPos = pos
      pos = tower.moveDown(currentShape, pos)
      movedDown = pos != lastPos
      nextJetIndex = (nextJetIndex + 1) % jetPattern.length
    }
  }

  private def parseInput: String = {
    Using(Source.fromResource("inputs/day17.txt")) { source =>
      source.getLines().next()
    }.get
  }
}

private final case class PositionInTower(left: Int, bottom: Int)

private class Tower(howManyShapesToFit: Int) {
  private val towerWidth = 7
  private val data = new Array[Array[Boolean]](5 * howManyShapesToFit + 5)
  private var highestRockY = 0
  data.indices.foreach(r => data(r) = Array.fill(towerWidth)(false))
  data(0) = Array.fill(towerWidth)(true)

  def height = highestRockY

  def addNew(shape: Shape): PositionInTower = {
    val bl = PositionInTower(2, highestRockY + 4)
    draw(shape, bl)
    bl
  }

  def moveLeft(shape: Shape, pos: PositionInTower): PositionInTower = {
    val canMove = shape.leftEdge(pos).forall(p => p.left > 0 && !data(p.bottom)(p.left - 1))
    if (!canMove) return pos
    val left = pos.copy(left = pos.left - 1)
    move(shape, pos, left)
    left
  }

  def moveRight(shape: Shape, pos: PositionInTower): PositionInTower = {
    val canMove = shape.rightEdge(pos).forall(p => p.left < towerWidth - 1 && !data(p.bottom)(p.left + 1))
    if (!canMove) return pos
    val right = pos.copy(left = pos.left + 1)
    move(shape, pos, right)
    right
  }

  def moveDown(shape: Shape, pos: PositionInTower): PositionInTower = {
    val canMove = shape.bottomEdge(pos).forall(p => !data(p.bottom - 1)(p.left))
    if (!canMove) return pos
    val down = pos.copy(bottom = pos.bottom - 1)
    move(shape, pos, down)
    updateHighestY()
    down
  }

  def print(): Unit = {
    ((highestRockY + 5)to 0 by -1).foreach { row =>
      val pr = (0 until towerWidth).map(col => if (data(row)(col)) "#" else ".").mkString
      println(pr)
    }
    println(s"Highest Y is $highestRockY")
  }

  private def move(shape: Shape, from: PositionInTower, to: PositionInTower): Unit = {
    clear(shape, from)
    draw(shape, to)
  }

  private def clear(shape: Shape, pos: PositionInTower): Unit = {
    shape.affectedPositions(pos).foreach { coords =>
      data(coords.bottom)(coords.left) = false
    }
  }

  private def draw(shape: Shape, pos: PositionInTower): Unit = {
    shape.affectedPositions(pos).foreach { coords =>
      data(coords.bottom)(coords.left) = true
    }
    updateHighestY()
  }

  private def updateHighestY(): Unit = {
    while (data(highestRockY).exists(x => x)) highestRockY += 1
    while (!data(highestRockY).exists(x => x)) highestRockY -= 1
  }
}

private final case class Shape(pixels: Array[Array[Boolean]]) {
  def affectedPositions(from: PositionInTower): IndexedSeq[PositionInTower] = {
    for {
      ri <- pixels.indices.reverse
      ydiff = pixels.length - 1 - ri
      (pix, ci) <- pixels(ri).zipWithIndex
      if pix
    } yield {
      PositionInTower(from.left + ci, from.bottom + ydiff)
    }
  }

  def rightEdge(from: PositionInTower): IndexedSeq[PositionInTower] = {
    for {
      ri <- pixels.length - 1 to 0 by -1
      ydiff = pixels.length - 1 - ri
    } yield {
      val x = (pixels(ri).length - 1 to 0 by -1).filter(pixels(ri)(_)).head
      PositionInTower(from.left + x, from.bottom + ydiff)
    }
  }

  def leftEdge(from: PositionInTower): IndexedSeq[PositionInTower] = {
    for {
      ri <- pixels.length - 1 to 0 by -1
      ydiff = pixels.length - 1 - ri
    } yield {
      val x = pixels(ri).indices.filter(pixels(ri)(_)).head
      PositionInTower(from.left + x, from.bottom + ydiff)
    }
  }

  def bottomEdge(from: PositionInTower): IndexedSeq[PositionInTower] = {
    for (ci <- pixels(0).indices) yield {
      val y = pixels.indices.reverse.filter(ri => pixels(ri)(ci)).head
      PositionInTower(from.left + ci, from.bottom + pixels.length - 1 - y)
    }
  }
}

private object Shapes {
  val horizonalLine: Shape = Shape(Array(Array(true, true, true, true)))

  val cross: Shape = Shape(
    Array(
      Array(false, true, false),
      Array(true, true, true),
      Array(false, true, false),
    )
  )

  val l: Shape = Shape(
    Array(
      Array(false, false, true),
      Array(false, false, true),
      Array(true, true, true),
    )
  )

  val verticalLine = Shape(
    Array(
      Array(true),
      Array(true),
      Array(true),
      Array(true),
    )
  )

  val square: Shape = Shape(
    Array(
      Array(true, true),
      Array(true, true),
    )
  )
}
