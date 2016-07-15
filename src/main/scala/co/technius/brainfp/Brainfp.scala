package co.technius.brainfp

import scala.collection.immutable.Seq

object Brainfp {
  def main(args: Array[String]): Unit = {
    val program = args.mkString
    val ast = Parser.parse(program)
    val res = ast.map(Interpreter.eval)
    res match {
      case Some(result) => print(result)
      case None => println("Error: Invalid program")
    }
  }
}
