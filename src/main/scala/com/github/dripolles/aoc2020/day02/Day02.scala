package com.github.dripolles.aoc2020.day02

import scala.io.Source
import scala.util.matching.Regex

object Day02 {
  val pattern: Regex = raw"(\d+)-(\d+) (\w): (\w+)".r
  val input = Source.fromResource("day02input.txt").getLines().map(parseLine).toSeq

  private def parseLine(line: String): PasswordEntry = {
    pattern.findFirstMatchIn(line) match {
      case Some(m) => PasswordEntry(
        m.group(1).toInt,
        m.group(2).toInt,
        m.group(3)(0),
        m.group(4),
      )
      case None => throw new RuntimeException("Bad input")
    }
  }

  lazy val validPolicy1Count: Int = input.count(_.followsPolicy1)
  lazy val validPolicy2Count: Int = input.count(_.followsPolicy2)
}

case class PasswordEntry(x: Int, y: Int, letter: Char, password: String) {
  lazy val followsPolicy1: Boolean = {
    val count = password.count { _ == letter }
    count >= x && count <= y
  }

  lazy val followsPolicy2: Boolean = {
    password(x-1) == letter ^ password(y-1) == letter
  }
}
