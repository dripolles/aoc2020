package com.github.dripolles.aoc2020.day07

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers.contain
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

import scala.io.Source



class Day07Test extends AnyFlatSpec {
  "simple containment lines" should "be parsed as relations" in {
    val simpleLine = "faded plum bags contain 5 wavy cyan bags."

    val expected = Seq(
      Relation("faded plum", 5, "wavy cyan")
    )
    val relations = Day07.parseRelations(simpleLine)
    relations should contain theSameElementsInOrderAs(expected)
  }

  "containment lines with multiple containments" should "be parsed as relations" in {
    val line = "vibrant brown bags contain 4 dark tan bags, 5 mirrored gray bags."

    val expected = Seq(
      Relation("vibrant brown", 4, "dark tan"),
      Relation("vibrant brown", 5, "mirrored gray")
    )
    val relations = Day07.parseRelations(line)
    relations should contain theSameElementsInOrderAs(expected)
  }

  "relations map" should "be built from relations" in {
    val relations = Seq(
      Relation("faded plum", 5, "wavy cyan"),
      Relation("vibrant brown", 4, "dark tan"),
      Relation("vibrant brown", 5, "mirrored gray")
    )

    val expected = Map[String, Relation](
      "faded plum" -> Relation("faded plum", 5, "wavy cyan"),
      "vibrant brown" -> Relation("vibrant brown", 4, "dark tan"),
      "vibrant brown" -> Relation("vibrant brown", 5, "mirrored gray")
    )

    val relationsMap = Day07.relationsMap(relations)
    relationsMap.relations should contain theSameElementsAs(expected)
  }

  "full example" should "work" in {
    val input = Source.fromResource("day07testinput.txt").getLines().toSeq
    val containedInMap = Day07.readAsContainedRelations(input)
    val reachable = containedInMap.reachable("shiny gold")
    assert(reachable.size == 5) // 4 plus the root
  }

  "full second part example" should "work" in {
    val input = Source.fromResource("day07testinput.txt").getLines().toSeq
    val containsMap = Day07.readAsContainsRelations(input)
    // All the children, plus the root
    assertResult(33) { containsMap.countAll("shiny gold") }

  }
}
