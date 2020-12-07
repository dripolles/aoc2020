package com.github.dripolles.aoc2020.day04

import com.github.dripolles.aoc2020.day04.Day04.Patterns

import scala.io.Source

object Day04 {
  val sourceLines = Source.fromResource("day04input.txt").getLines().toSeq

  def parse(lines: Seq[String]): Seq[PassportData] = {
    val z = (Seq.empty[PassportData], PassportData())
    val parsed = lines.foldLeft(z) { (acc, line) =>
      if (line == "") {
        (acc._1 :+ acc._2, PassportData())
      } else {
        (acc._1, acc._2.merge(parseLine(line)))
      }
    }

    if (parsed._2.data.isEmpty) {
      parsed._1
    } else {
      parsed._1 :+ parsed._2
    }
  }

  private def parseLine(line: String): Map[String, String] = {
    line.split(" ").foldLeft(Map.empty[String, String]) { (m, pair) =>
      val parts = pair.split(":")
      m + (parts(0) -> parts(1))
    }
  }

  object Patterns {
    val year = raw"(\d{4})".r.anchored
    val height = raw"(\d+)((cm)|(in))".r.anchored
    val hairColor = raw"#[0-9a-f]{6}".r.anchored
    val passportId = raw"[0-9]{9}".r.anchored
  }
}

case class PassportData(data: Map[String, String] = Map.empty) {
  def +(keyValue: (String, String)): PassportData = {
    copy(data = data + keyValue)
  }

  def merge(newData: Map[String, String]): PassportData = {
    copy(data = data ++ newData)
  }

  lazy val isValidFields: Boolean = {
    data.keys.size == 8 || (data.keys.size == 7 && !data.keySet.contains("cid"))
  }

  lazy val isValid: Boolean = {
    if (isValidFields) {
      validYear(data("byr"), 1920, 2002) &&
        validYear(data("iyr"), 2010, 2020) &&
        validYear(data("eyr"), 2020, 2030) &&
        validHeight(data("hgt")) &&
        validEyeColor(data("ecl")) &&
        Patterns.hairColor.matches(data("hcl")) &&
        Patterns.passportId.matches(data("pid"))
    } else {
      false
    }
  }

  private def validYear(value: String, min: Int, max: Int): Boolean = {
    Patterns.year.findFirstIn(value).exists { strYear =>
      val year = strYear.toInt
      year >= min && year <= max
    }
  }

  private def validHeight(value: String): Boolean = {
    Patterns.height.findFirstMatchIn(value).exists { m =>
      val height = m.group(1).toInt
      m.group(2) match {
        case "cm" => height >= 150 && height <= 193
        case "in" => height >= 59 && height <= 76
      }
    }
  }

  private def validEyeColor(value: String): Boolean = {
    Set("amb", "blu", "brn", "gry", "grn", "hzl", "oth").contains(value)
  }
}
