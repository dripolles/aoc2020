package com.github.dripolles.aoc2020.day08

import scala.reflect.ClassTag
import scala.reflect.runtime.universe._

sealed trait Op
case class NoOp(unused: Int) extends Op
case class Jmp(offset: Int) extends Op
case class Acc(delta: Int) extends Op

case class ProgramOp(op: Op, visited: Boolean = false)

case class ProgramState(
  ops: Seq[ProgramOp],
  pos: Int = 0,
  acc: Int = 0
) {

  def isInLoop: Boolean = ops.lift(pos).fold(false) { _.visited }
  def isDone: Boolean = pos == ops.length
  def isOutOfBounds: Boolean = pos >= ops.length

  def shouldStop: Boolean = {
    isInLoop || isDone || isOutOfBounds
  }

  def run: Seq[ProgramState] = {
    val (done, toDo) = LazyList.iterate(this) { state =>
      state.runStep
    }.span { state =>
      !state.shouldStop
    }
    // We need the state that will not be executed, as it contains the latest acc and pos
    done #::: toDo.take(1)
  }

  def runStep: ProgramState = {
    val programOp = ops(pos)
    val (nextPos, accDelta) = programOp.op match {
      case NoOp(_) => (pos + 1, 0)
      case Acc(delta) => (pos + 1, delta)
      case Jmp(offset) => (pos + offset, 0)
    }
    ProgramState(
      ops = ops.updated(pos, programOp.copy(visited = true)),
      pos = nextPos,
      acc = acc + accDelta,
    )
  }

  def findCorrectFinalState: Option[ProgramState] = {
    replacedInitialStates.map { state =>
      state.run.last
    }.find { lastState =>
      lastState.pos == lastState.ops.length
    }
  }

  def replacedInitialStates: LazyList[ProgramState] = {
    val replacedNoOp = replacedStates[NoOp] { noOp => Jmp(noOp.unused) }
    val replacedJmp = replacedStates[Jmp] { jmp => NoOp(jmp.offset) }
    replacedNoOp #::: replacedJmp
  }

  private def replacedStates[T](mapper: T => Op)(implicit tag: ClassTag[T]): LazyList[ProgramState] = {
    val opPositions = ops.zipWithIndex.filter { case (p, _) => p.op.getClass == tag.runtimeClass }.map(_._2)
    LazyList.from(opPositions).map { idx =>
      val toReplace = ops(idx).op.asInstanceOf[T]
      val replaced = mapper(toReplace)
      copy(
        ops = ops.updated(idx, ProgramOp(replaced)),
      )
    }
  }
}


object Day08 {
  def parseInput(lines: Seq[String]): ProgramState = {
    val programOps = lines.map { line =>
      val parts = line.split(" ", 2)
      val (opStr, value) = (parts(0), parts(1).toInt)
      val op = opStr match {
        case "nop" => NoOp(value)
        case "acc" => Acc(value)
        case "jmp" => Jmp(value)
      }
      ProgramOp(op)
    }

    ProgramState(ops = programOps)
  }
}
