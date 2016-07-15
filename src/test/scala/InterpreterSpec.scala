import org.scalatest._

import co.technius.brainfp._

class InterpreterSpec extends FlatSpec with Matchers {
  "Interpreter" should "parse a valid expression" in {
    val astOpt = Interpreter.parse("+[>+<-].")
    astOpt should not be empty
  }

  it should "run Hello World" in {
    /*
     * Brainfuck "Hello World" posted by StackExchange Programming Puzzles &
     * Code Golf user primo. http://codegolf.stackexchange.com/a/68494
     */
    val program = """--->->->>+>+>>+[++++[>+++[>++++>-->+++<<<-]
                     <-]<+++]>>>.>-->-.>..+>++++>+++.+>-->[>-.<<]"""
    val resultOpt = Interpreter.parse(program).map(Interpreter.eval)
    resultOpt should === (Some("Hello, World!"))
  }

  it should "overflow an int" in {
    val program = "++++++++[>++++++++<-]."
    val resultOpt = Interpreter.parse(program).map(Interpreter.eval)
    resultOpt should === (Some(0.toByte.toChar.toString))
  }

  it should "underflow an int" in {
    val program = "-."
    val resultOpt = Interpreter.parse(program).map(Interpreter.eval)
    resultOpt should === (Some(255.toByte.toChar.toString))
  }
}
