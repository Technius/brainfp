package co.technius.brainfp

import scala.collection.mutable.StringBuilder

import Command._

object Interpreter {

  def eval(commands: Seq[Command]): String = {
    val stack = Array.fill[Byte](100000)(0)
    val output = new StringBuilder()
    var ptr = 0
    def doEval(next: Seq[Command]): Unit = {
      next foreach {
        case RightShiftPointer => ptr = ptr + 1
        case LeftShiftPointer  => ptr = ptr - 1
        case IncrementCell => stack(ptr) = (stack(ptr) + 1).toByte
        case DecrementCell => stack(ptr) = (stack(ptr) - 1).toByte
        case OutputCharacter => output.append(stack(ptr).toChar)
        case InputCharacter =>
          val input = io.StdIn.readChar().toByte
          stack(ptr) = input
        case Loop(commands) =>
          while (stack(ptr) > 0) doEval(commands)
      }
    }
    doEval(commands)
    output.toString
  }

  def step(state: State, next: Seq[Command]): (State, EvalStep) = {
    if (next.isEmpty) {
      state -> Finish
    } else {
      val ptr = state.ptr
      val stack = state.stack
      next.head match {
        case RightShiftPointer => state.copy(ptr = ptr + 1) -> Resume
        case LeftShiftPointer  => state.copy(ptr = ptr - 1) -> Resume
        case IncrementCell =>
          val updStack = stack.updated(ptr, (stack(ptr) + 1).toByte)
          state.copy(stack = updStack) -> Resume
        case DecrementCell =>
          val updStack = stack.updated(ptr, (stack(ptr) - 1).toByte)
          state.copy(stack = updStack) -> Resume
        case OutputCharacter => state -> Output(stack(ptr).toChar)
        case InputCharacter =>
          state -> AwaitInput
        case Loop(commands) =>
          state -> (if (stack(ptr) > 0) Execute(commands) else Resume)
      }
    }
  }

}

sealed trait EvalStep
case class Execute(commands: Seq[Command]) extends EvalStep
case class Output(output: Char) extends EvalStep
case object AwaitInput extends EvalStep
case object Resume extends EvalStep
case object Finish extends EvalStep

case class State(stack: Seq[Byte], ptr: Int)
