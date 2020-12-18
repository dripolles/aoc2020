package com.github.dripolles.aoc2020.day09

import scala.annotation.tailrec

object Day09 {

  def parseInput(lines: Seq[String], preambleSize: Int): (Seq[BigInt], Seq[BigInt]) = {
    lines.map {s => BigInt(s) }.splitAt(preambleSize)
  }

  def firstNotInSums(preamble: Seq[BigInt], xs: Seq[BigInt]): Option[BigInt] = {
    val processedPreamble = processPreamble(preamble)
    processedPreamble.findFirstNotInSums(xs)
  }

  def processPreamble(preamble: Seq[BigInt]): CalcState = {
    val (two, rest) = preamble.splitAt(2)
    val first = CalcState(
      sums = Set(two(0) + two(1)),
      two.toSet,
      Set.empty
    )

    val afterPreamble = rest.foldLeft(first) { (state, n) => state.add(n) }

    afterPreamble.copy(notInSums = Set.empty)
  }

  def findContiguousSum(target: BigInt, xs: Seq[BigInt]): Option[Set[BigInt]] = {
    xs.view.tails.map {
      tail => findContiguousSumFromHead(target, tail.toSeq)
    }.find { _.isDefined
    }.map { _.get.added }
  }

  def findContiguousSumFromHead(target: BigInt, xs: Seq[BigInt]): Option[Candidate] = {
    val start = Candidate(Set.empty, 0, xs)
    start.findTarget(target)
  }
}

case class CalcState(
  sums: Set[BigInt] = Set.empty,
  numbers: Set[BigInt] = Set.empty,
  notInSums: Set[BigInt] = Set.empty
) {

  @tailrec
  final def findFirstNotInSums(xs: Seq[BigInt]): Option[BigInt] = {
    if (notInSums.nonEmpty) {
      return Some(notInSums.head)
    }

    xs.headOption match {
      case None => None
      case Some(n) => add(n).findFirstNotInSums(xs.tail)
    }
  }

  def add(n: BigInt): CalcState = {
    val updatedSums = numbers.foldLeft(sums) { (s, x) => s + (x + n) }
    CalcState(
      updatedSums,
      numbers + n,
      if (sums.contains(n)) notInSums else notInSums + n,
    )
  }
}

case class Candidate(added: Set[BigInt], sum: BigInt, pending: Seq[BigInt]) {

  @tailrec
  final def findTarget(target: BigInt): Option[Candidate] = {
    if (sum == target) {
      Some(this)
    } else {
      step(target) match {
        case None => None
        case Some(c) => c.findTarget(target)
      }
    }
  }

  def step(target: BigInt): Option[Candidate] = {
    pending.headOption.flatMap { x =>
      val newSum = sum + x
      if (newSum > target) {
        None
      } else {
        Some(Candidate(added + x, newSum, pending.tail))
      }
    }
  }
}
