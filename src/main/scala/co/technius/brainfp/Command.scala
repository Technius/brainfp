package co.technius.brainfp

import scala.collection.immutable.Seq

sealed trait Command
object Command {
  case object RightShiftPointer            extends Command
  case object LeftShiftPointer             extends Command
  case object IncrementCell                extends Command
  case object DecrementCell                extends Command
  case object OutputCharacter              extends Command
  case object InputCharacter               extends Command
  case class  Loop(commands: Seq[Command]) extends Command

  def prettyPrint(cmd: Seq[Command]): Seq[String] = {
    cmd flatMap {
      case Loop(cmds) =>
        Seq("Loop") ++ prettyPrint(cmds).map("  " + _)
      case c => Seq(c.toString)
    }
  }
}
