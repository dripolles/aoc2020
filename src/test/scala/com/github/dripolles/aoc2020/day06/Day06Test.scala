package com.github.dripolles.aoc2020.day06

import org.scalatest.flatspec.AnyFlatSpec

import scala.io.Source

class Day06Test extends AnyFlatSpec {
  "group countAll" should "count the elements present in all entries" in {
    val g = Group(
      Seq(
        Set('a', 'b', 'c', 'd'),
        Set('c', 'x', 'y', 'z'),
      )
    )

    assertResult(1) { g.countAll }
  }

  "group countAll" should "work with the example input" in {
    val input = Source.fromResource("day06testinput.txt")
    val groups = Day06.parseGroups(input.getLines().toSeq)

    val expected = Seq(3, 0, 1, 1, 1)
    val counts = groups.map(_.countAll)
    assert(expected == counts)

    assertResult(6) { groups.foldLeft(0) { (sum, g) => sum + g.countAll } }


  }
}
