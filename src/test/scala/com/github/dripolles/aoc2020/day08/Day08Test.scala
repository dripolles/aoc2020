package com.github.dripolles.aoc2020.day08

import org.scalatest.flatspec.AnyFlatSpec

import scala.io.Source

class Day08Test extends AnyFlatSpec {
  val input = Source.fromResource("day08testinput.txt").getLines().toSeq
  val programState = Day08.parseInput(input)
  "example input for first part" should "work as expected" in {
    val runStates = programState.run
    assert(runStates.length == 8)
    assert(runStates.last.acc == 5)
  }

  "example input for second part" should "work as expected" in {
    val finalState = programState.findCorrectFinalState.get
    assert(finalState.acc == 8)
  }
}
