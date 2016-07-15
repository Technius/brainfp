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
}
