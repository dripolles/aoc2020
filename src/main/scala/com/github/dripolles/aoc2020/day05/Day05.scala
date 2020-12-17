package com.github.dripolles.aoc2020.day05

import scala.io.Source

case class Seat(row: Int, column: Int) {
  val id: Int = row * 8 + column
}

object Day05 {
  def input: Iterator[String] = Source.fromResource("day05input.txt").getLines()

  def decode(boardingPass: String): Seat = {
    val row = decodePart(boardingPass.substring(0, 7), (0, 127),'B', 'F')
    val column = decodePart(boardingPass.substring(7, 10), (0, 7),'R', 'L')

    Seat(row, column)
  }

  private def decodePart(part: String, start: (Int, Int), higher: Char, lower: Char): Int = {
    val smallestRange = part.foldLeft(start) { (range, c) =>
      val half = (range._2 - range._1) / 2
      c match {
        case `higher` => (range._1 + half + 1, range._2)
        case `lower` => (range._1, range._1 + half)
        case other => throw new RuntimeException(s"unexpedted character $other")
      }
    }

    smallestRange._1
  }
}
