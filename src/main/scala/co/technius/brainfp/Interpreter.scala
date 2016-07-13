package co.technius.brainfp

import scala.collection.mutable.StringBuilder

import Command._

object Interpreter {
  def parse(source: String): Option[Stream[Command]] = {
    def run(cmds: Stream[Command], next: String): Option[Stream[Command]] = {
      if (next.isEmpty) {
        Some(cmds)
      } else {
        next.head match {
          case '>' => run(cmds :+ RightShiftPointer, next.tail)
          case '<' => run(cmds :+ LeftShiftPointer,  next.tail)
          case '+' => run(cmds :+ IncrementCell,     next.tail)
          case '-' => run(cmds :+ DecrementCell,     next.tail)
          case '.' => run(cmds :+ OutputCharacter,   next.tail)
          case ',' => run(cmds :+ InputCharacter,    next.tail)
          case '[' =>
            @annotation.tailrec
            def getLoop(lvl: Int, acc: String, n: String): Option[(String, String)] = {
              if (n.isEmpty || !n.contains(']')) None
              else if (n.head == '[') getLoop(lvl + 1, acc + '[', n.tail)
              else if (n.head == ']' && lvl == 0) Some(acc -> n.tail)
              else if (n.head == ']') getLoop(lvl - 1, acc + ']', n.tail)
              else getLoop(lvl, acc + n.head, n.tail)
            }
            val loopOpt = getLoop(0, "", next.tail)
            for {
              (loop, tail) <- loopOpt
              inLoop       <- run(Stream.empty, loop)
              res          <- run(cmds :+ Loop(inLoop), tail)
            } yield {
              res
            }
          case _ => run(cmds, next.tail)
        }
      }
    }
    run(Stream.empty[Command], source)
  }

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
