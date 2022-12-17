package com.adventofcode.twentywentytwo

import scala.collection.mutable.ListBuffer
import scala.io.Source
import scala.util.Using

object Day13 {
  def main(args: Array[String]): Unit = {
    val sumOfIndices = Using(Source.fromResource("inputs/day13.txt")) { source =>
      source.getLines().grouped(3).zipWithIndex.map { case (pair, index) =>
          Element.compare(parseList(pair.head), parseList(pair(1))) match {
            case x if x < 0 => index + 1
            case _ => 0
          }
      }.filter(_ > 0).sum
    }.get

    println(sumOfIndices)
  }

  def parseNumber(str: String): NumberElement = NumberElement(str.trim.toInt)

  def parseList(line: String): ListElement = {
    val elementsString = line.stripListDelimiters
    val elements = new ListBuffer[Element]()
    var idx = 0

    def listElementBuffer: String = {
      val sb = new StringBuilder("[")
      var cntPar = 1
      idx += 1

      while (cntPar > 0) {
        val ch = elementsString(idx)
        sb.addOne(ch)
        idx += 1
        if (ch == '[') cntPar += 1
        else if (ch == ']') cntPar -= 1
      }

      if (elementsString.endOfElement(idx)) idx += 1
      sb.toString
    }

    def numberElementBuffer: String = {
      val sb = new StringBuilder()
      while (elementsString.isDigit(idx)) {
        sb.addOne(elementsString.charAt(idx))
        idx += 1
      }
      idx += 1
      sb.toString
    }

    while (idx < elementsString.length) {
      val element =
        if (elementsString.beginOfList(idx)) parseList(listElementBuffer)
        else parseNumber(numberElementBuffer)
      elements.addOne(element)
    }

    ListElement(elements.toSeq)
  }

  private implicit class StringExtensions(str: String) {
    def beginOfList(idx: Int): Boolean = idx < str.length && str.charAt(idx) == '['
    def isDigit(idx: Int): Boolean = idx < str.length && !Seq(']', ',').contains(str(idx))
    def endOfElement(idx: Int): Boolean = idx < str.length && str(idx) == ','
    def stripListDelimiters: String = str.take(str.length - 1).substring(1)
  }
}

sealed trait Element

final case class NumberElement(value: Int) extends Element

final case class ListElement(values: Seq[Element]) extends Element {
  def size: Int = values.size
  def apply(i: Int): Element = values(i)
}
object ListElement {
  def of(numberElement: NumberElement) = ListElement(Seq(numberElement))
}

object Element {
  /**
   * @return 0 if equal, <0 if right order and >0 if incorrect order
   */
  def compare(element1: Element, element2: Element): Int = {
    (element1, element2) match {
      case (num1: NumberElement, num2: NumberElement) =>
        num1.value - num2.value
      case (num1: NumberElement, list2: ListElement) =>
        compare(ListElement.of(num1), list2)
      case (list1: ListElement, num2: NumberElement) =>
        compare(list1, ListElement.of(num2))
      case (list1: ListElement, list2: ListElement) =>
        (0 until Math.max(list1.size, list2.size)).to(LazyList)
          .map(compareListItem(list1, list2, _))
          .find(_ != 0)
          .getOrElse(0)
    }
  }

  def compareListItem(list1: ListElement, list2: ListElement, i: Int): Int = {
    if (i == list1.size && i == list2.size) 0 // they are identical
    else if (i >= list1.size) -1 // left side is smaller
    else if (i >= list2.size) 1 // right side is smaller
    else compare(list1(i), list2(i))
  }
}