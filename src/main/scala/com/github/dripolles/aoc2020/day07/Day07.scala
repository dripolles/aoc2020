package com.github.dripolles.aoc2020.day07

case class Relation(thisOne: String, count: Int, other: String) {
  def inverse: Relation = Relation(other, count, thisOne)
}

case class RelationsMap(relations: Map[String, Set[Relation]] = Map.empty) {
  def addRelation(relation: Relation): RelationsMap = {
    val newRelations = relations.updatedWith(relation.thisOne) {
      case Some(s) => Some(s + relation)
      case None => Some(Set(relation))
    }

    RelationsMap(newRelations)
  }

  def reachable(colour: String): Set[String] = {
    reachableWithVisited(colour, Set.empty)
  }

  private def reachableWithVisited(colour: String, visited: Set[String]): Set[String] = {
    relations.lift(colour) match {
      case None => visited + colour
      case Some(rels) => {
        rels.foldLeft(visited + colour) { (newVisited, rel) =>
          if (newVisited.contains(rel.other)) {
            newVisited
          } else {
            newVisited ++ reachableWithVisited(rel.other, newVisited)
          }
        }
      }
    }
  }

  def countAll(colour: String): Int = {
    val childrenCount = relations.lift(colour) match {
      case None =>  0
      case Some(rels) =>
        rels.foldLeft(0) { (count, rel) =>
          count + rel.count * countAll(rel.other)
        }
    }
    childrenCount + 1
  }
}

object Day07 {
  private val relationHeadPattern = raw"(\w+ \w+) bags contain (.*)".r.anchored
  private val relationTailPattern = raw"(\d+) (\w+ \w+) bags?\.?".r.anchored

  def readAsContainedRelations(lines: Seq[String]): RelationsMap = {
    val relations = parseAllRelations(lines).map { _.inverse }
    relationsMap(relations)
  }

  def readAsContainsRelations(lines: Seq[String]): RelationsMap = {
    val relations = parseAllRelations(lines)
    relationsMap(relations)
  }

  def parseAllRelations(lines: Seq[String]): Seq[Relation] = {
    lines.flatMap { line => parseRelations(line) }
  }
  def parseRelations(line: String): Seq[Relation] = {
    val (colour, relDescs) = relationHeadPattern.findFirstMatchIn(line) match {
      case Some(m) => (m.group(1), m.group(2))
      case None => throw new RuntimeException(s"wrong line format: $line")
    }
    relDescs match {
      case "no other bags." => Seq.empty
      case _ =>
        relDescs.split(", ").map { part =>
          relationTailPattern.findFirstMatchIn(part) match {
            case Some(m) => Relation(colour, m.group(1).toInt, m.group(2))
            case None => throw new RuntimeException(s"wrong part format: $part")
          }
        }
    }
  }

  def relationsMap(relations: Seq[Relation]): RelationsMap  = {
    relations.foldLeft(RelationsMap()) { (m, relation) =>
      m.addRelation(relation)
    }
  }
}
