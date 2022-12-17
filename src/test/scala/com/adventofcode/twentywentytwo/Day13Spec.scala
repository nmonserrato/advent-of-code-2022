package com.adventofcode.twentywentytwo

import com.adventofcode.twentywentytwo.Day13.{parseList, parseNumber}

class Day13Spec extends org.specs2.mutable.Specification {
  "should parse a list of ints" in {
    parseList("[1,1,5,1,1]") should_== ListElement(Seq(1, 1, 5, 1, 1).map(NumberElement))
  }

  "should parse one level nested lists" in {
    parseList("[[1],[2,3,4]]") should_== ListElement(Seq(
      ListElement(Seq(NumberElement(1))),
      ListElement(Seq(NumberElement(2), NumberElement(3), NumberElement(4)))
    ))
  }

  "should parse list of one element" in {
    parseList("[9]") should_== ListElement(Seq(NumberElement(9)))
  }

  "should parse an empty list" in {
    parseList("[]") should_== ListElement(Nil)
  }

  "should parse a single nested list" in {
    parseList("[[8,7,6]]") should_== ListElement(Seq(ListElement(Seq(8, 7, 6).map(NumberElement))))
  }

  "should parse a single nested list" in {
    parseList("[[[]]]") should_== ListElement(Seq(ListElement(Seq(ListElement(Nil)))))
  }

  "should parse a complex list" in {
    parseList("[1,[2,[3,[4,[5,6,7]]]],8,9]") should_==
      ListElement(Seq(NumberElement(1), ListElement(Seq(NumberElement(2), ListElement(Seq(NumberElement(3), ListElement(Seq(NumberElement(4), ListElement(Seq(5, 6, 7).map(NumberElement)))))))), NumberElement(8), NumberElement(9)))
  }

  "should compare two lists of ints" in {
    val list1 = parseList("[1,1,3,1,1]")
    val list2 = parseList("[1,1,5,1,1]")

    Element.compare(list1, list2) should beLessThan(0)
  }

  "should compare two small lists of lists" in {
    val list1 = parseList("[[1],[2,3,4]]")
    val list2 = parseList("[[1],4]")

    Element.compare(list1, list2) should beLessThan(0)
  }

  "should compare two unbalances lists of lists" in {
    val list1 = parseList("[9]")
    val list2 = parseList("[[8,7,6]]")

    Element.compare(list1, list2) should beGreaterThan(0)
  }

  "should compare two unbalances lists of lists - part 2" in {
    val list1 = parseList("[7, 7, 7, 7]")
    val list2 = parseList("[7, 7, 7]")

    Element.compare(list1, list2) should beGreaterThan(0)
  }

  "should compare an empty list with another list" in {
    val list1 = parseList("[]")
    val list2 = parseList("[3]")

    Element.compare(list1, list2) should beLessThan(0)
  }

  "should compare a wtf" in {
    val list1 = parseList("[[[]]]")
    val list2 = parseList("[[]]")

    Element.compare(list1, list2) should beGreaterThan(0)
  }

  "should compare a super duper complex pair of lists" in {
    val list1 = parseList("[1,[2,[3,[4,[5,6,7]]]],8,9]")
    val list2 = parseList("[1,[2,[3,[4,[5,6,0]]]],8,9]")

    Element.compare(list1, list2) should beGreaterThan(0)
  }
}
