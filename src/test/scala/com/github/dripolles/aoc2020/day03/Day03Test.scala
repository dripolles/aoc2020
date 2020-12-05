package com.github.dripolles.aoc2020.day03

import org.scalatest.flatspec.AnyFlatSpec


class Day03Test extends AnyFlatSpec {
  "an area full of trees" should "traverse with nRows trees" in {
    val area = (1 to 100).map { _ =>
      (1 to 10).map {_ =>  Tree }
    }

    val state = TobogganState(0, 0, area)

    (1 to 25).foreach { rowDelta =>
    val expected = Math.ceil(100.toFloat / rowDelta).toInt
    assertResult(expected) {
      state.countTrees {_.move(rowDelta, 3)}}
    }
  }
}
