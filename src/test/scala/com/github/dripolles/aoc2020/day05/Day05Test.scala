package com.github.dripolles.aoc2020.day05

import org.scalatest.flatspec.AnyFlatSpec

class Day05Test extends AnyFlatSpec {
  private case class TestEntry(boardingPass: String, seat: Seat, id: Int)
  "example cases from description" should "work as expected" in {

    /*
    BFFFBBFRRR: row 70, column 7, seat ID 567.
    FFFBBBFRRR: row 14, column 7, seat ID 119.
    BBFFBBFRLL: row 102, column 4, seat ID 820.
     */

    val entries = Seq(
      TestEntry("BFFFBBFRRR", Seat(70, 7), 567),
      TestEntry("FFFBBBFRRR", Seat(14, 7), 119),
      TestEntry("BBFFBBFRLL", Seat(102, 4), 820),

    )
    entries.foreach { entry =>
      val seat = Day05.decode(entry.boardingPass)
      assert(seat == entry.seat)
      assert(seat.id == entry.id)
    }
  }
}
