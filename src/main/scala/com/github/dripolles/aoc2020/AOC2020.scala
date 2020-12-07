package com.github.dripolles.aoc2020

import com.github.dripolles.aoc2020.day01.Day01
import com.github.dripolles.aoc2020.day02.Day02
import com.github.dripolles.aoc2020.day03.TobogganState
import com.github.dripolles.aoc2020.day04.Day04

object AOC2020 {
  def main(args: Array[String]): Unit = {
    day04
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
}
