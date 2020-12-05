package com.github.dripolles.aoc2020.day03

import scala.io.Source

sealed trait Point
object Tree extends Point
object Water extends Point

object TobogganState {
  def read() = {
    val areaDef = Source.fromResource("day03input.txt").getLines().map(toPoints).toSeq
    TobogganState(0,0, areaDef)
  }

  private def toPoints(s: String): Seq[Point] = {
    s.map {
      case '.' => Water
      case '#'=> Tree
      case otherwise => throw new RuntimeException(s"invalid input: $otherwise")
    }
  }
}

case class TobogganState(row: Int, col: Int, area: Seq[Seq[Point]]) {
  def move(rowDelta: Int, colDelta: Int): Option[TobogganState] = {
    val newRow = row + rowDelta
    area.lift(newRow).
      map { rowSeq =>
        val newCol = (col + colDelta) % rowSeq.length
        copy(newRow, newCol, area)
      }
  }

  private def printDebug(row: Int, col: Int) = {
    println(row, col, area(row)(col))
    val repr = area(row).zipWithIndex.map { case (point, pos) =>
      point match {
        case Tree => if (pos == col) "X" else "#"
        case Water => if (pos == col) "O" else "."
      }
    }.mkString("")
    println(repr)
  }

  def point: Point = area(row)(col)

  def traverse(movement: TobogganState => Option[TobogganState]): Seq[TobogganState] = {
    val rest = movement(this).
      map {next => next.traverse(movement)}.
      getOrElse(Seq.empty)
    this +: rest
  }

  def countTrees(movement: TobogganState => Option[TobogganState]): Int = {
    traverse(movement).count { _.point == Tree }
  }
}
