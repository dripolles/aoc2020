package com.github.dripolles.aoc2020

import com.github.dripolles.aoc2020.day01.Day01
import com.github.dripolles.aoc2020.day02.Day02
import com.github.dripolles.aoc2020.day03.TobogganState
import com.github.dripolles.aoc2020.day04.Day04
import com.github.dripolles.aoc2020.day05.Day05
import com.github.dripolles.aoc2020.day06.Day06
import com.github.dripolles.aoc2020.day07.Day07
import com.github.dripolles.aoc2020.day08.Day08

import scala.io.Source

object AOC2020 {
  def main(args: Array[String]): Unit = {
    day08
  }

  def day01 = {
    val day01 = new Day01(Day01.input)
    println(day01.solution1)
    println(day01.solution2)
  }

  def day02 = {
    println(Day02.validPolicy1Count)
    println(Day02.validPolicy2Count)
  }

  def day03 = {
    val state = TobogganState.read()
    val count = state.countTrees { _.move(1, 3) }
    println(count)

    val count_1_1: BigInt = state.countTrees { _.move(1, 1) }
    val count_1_3: BigInt = state.countTrees { _.move(1, 3) }
    val count_1_5: BigInt = state.countTrees { _.move(1, 5) }
    val count_1_7: BigInt = state.countTrees { _.move(1, 7) }
    val count_2_1: BigInt = state.countTrees { _.move(2, 1) }

    println(count_1_1 * count_1_3 * count_1_5 * count_1_7 * count_2_1)
  }

  def day04 = {
    val passports = Day04.parse(Day04.sourceLines)
    val validFieldsCount = passports.count { _.isValidFields }
    println(validFieldsCount)

    val validCount = passports.count { _.isValid }
    println(validCount)
  }

  def day05 = {
    val seats = Day05.input. map(Day05.decode).toSeq
    val maxSeat = seats.maxBy { seat => seat.id }
    println(maxSeat.id)

    val minSeat = seats.minBy { seat => seat.id }
    val fullAircraft = Set.range(minSeat.id, maxSeat.id + 1)
    val occupied = seats.foldLeft(Set.empty[Int]) { (allSeats, seat) => allSeats + seat.id }
    println(fullAircraft -- occupied)
  }

  def day06 = {
    val groups = Day06.parseGroups(Day06.input)
    val sumCountsAny = groups.foldLeft(0) { (sum, g) => sum + g.countAny }
    println(sumCountsAny)

    val sumCountsAll = groups.foldLeft(0) { (sum, g) => sum + g.countAll }
    println(sumCountsAll)
  }
  def day07 = {
    val input = Source.fromResource("day07input.txt").getLines().toSeq
    val containedInMap = Day07.readAsContainedRelations(input)
    val reachable = containedInMap.reachable("shiny gold")
    println(reachable.size - 1)

    val containsMap = Day07.readAsContainsRelations(input)
    val totalCount = containsMap.countAll("shiny gold")
    println(totalCount - 1)
  }

  def day08 = {
    val input = Source.fromResource("day08input.txt").getLines().toSeq
    val programState = Day08.parseInput(input)
    val runStates = programState.run
    println(runStates.last.acc)

    val finalState = programState.findCorrectFinalState.get
    println(finalState.acc)
  }
}
