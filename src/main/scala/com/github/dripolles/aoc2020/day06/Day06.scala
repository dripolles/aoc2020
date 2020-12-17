package com.github.dripolles.aoc2020.day06

import scala.io.Source


case class Group(responses: Seq[Set[Char]] = Seq.empty) {
  def countAny: Int = {
    val allResponses = responses.reduceLeft { (s1, s2) => s1 ++ s2 }
    allResponses.size
  }

  def countAll: Int = {
    val allResponses = responses.reduceLeft { (s1, s2) => s1 & s2 }
    allResponses.size
  }

  def add(response: Set[Char]): Group = {
    copy(responses = responses :+ response)
  }
}

object Day06 {
  val input = Source.fromResource("day06input.txt").getLines().toSeq

  private case class PartialParse(parsed: Seq[Group] = Seq.empty, current: Group = Group())
  def parseGroups(lines: Seq[String]): Seq[Group] = {
    val parsed = lines.foldLeft(PartialParse()) { (parse, line) =>
      line match {
        case "" => PartialParse(parse.parsed :+ parse.current, Group())
        case _ => parse.copy(current = parse.current.add(line.toSet))
      }
    }
    parsed.parsed :+ parsed.current
  }
}
