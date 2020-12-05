package com.github.dripolles.aoc2020.day01

import scala.io.Source

object Day01 {
  val input = Source.fromResource("day01input.txt").getLines().map(_.toInt).toSeq

}
class Day01(input: Seq[Int]) {
  def solution1: Int = {
    val pair = pairs(input).find { case (x,y) => x + y == 2020 }.get
    pair._1 * pair._2
  }

  def solution2: Int = {
   val triplet = triplets(input).find { case (x,y,z) => x+y+z == 2020}.get
    triplet._1 * triplet._2 * triplet._3
  }

  def pairs[T](inputSeq: Seq[T]): Seq[(T, T)] = {
    inputSeq.tails.foldLeft(Seq.empty[(T, T)]) { (acc, seq) =>
      if (seq.nonEmpty) {
        acc ++ seq.tail.map { (seq.head, _)}
      } else {
        acc
      }
    }
  }

  def triplets[T](inputSeq: Seq[T]): Seq[(T, T, T)] = {
    inputSeq.tails.foldLeft(Seq.empty[(T, T, T)]){ (acc, seq) =>
      if (seq.length >= 2) {
        val tailPairs = pairs(seq.tail)
        acc ++ tailPairs.map { x => (seq.head, x._1, x._2) }
      } else {
        acc
      }
    }
  }
}
