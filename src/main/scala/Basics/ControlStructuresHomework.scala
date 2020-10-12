package Basics
import scala.io.Source

object ControlStructuresHomework {
  // Homework

  // Create a command line application that reads various "commands" from the
  // stdin, evaluates them, and writes output to stdout.

  // Commands are:

  //   divide 4 5
  // which should output "4 divided by 5 is 0.8"

  //   sum 5 5 6 8.5
  // which should output "the sum of 5 5 6 8.5 is 24.5"

  //   average 4 3 8.5 4
  // which should output "the average of 4 3 8.5 4 is 4.875"

  //   min 4 -3 -17
  // which should output "the minimum of 4 -3 -17 is -17"

  //   max 4 -3 -17
  // which should output "the maximum of 4 -3 -17 is 4"

  // In case of commands that cannot be parsed or calculations that cannot be performed,
  // output a single line starting with "Error: "

  sealed trait Command
  object Command {
    final case class Divide(dividend: Double, divisor: Double) extends Command
    final case class Sum(numbers: List[Double]) extends Command
    final case class Average(numbers: List[Double]) extends Command
    final case class Min(numbers: List[Double]) extends Command
    final case class Max(numbers: List[Double]) extends Command
  }

  final case class ErrorMessage(value: String){
    override def toString(): String = value
  }

  sealed trait Result
  // adjust Result as required to match requirements
  final case class CommandResult(command: Command, value: Double) extends Result

  def parseCommand(x: String): Either[ErrorMessage, Command] = {
     // implement this method
    // Implementation hints:
    // You can use String#split, convert to List using .toList, then pattern match on:
    //   case x :: xs => ???

    // Consider how to handle extra whitespace gracefully (without errors).

    x.trim.split(" ").toList match
    {
      case _ :: xs if xs.map(_.toDoubleOption).contains(None) =>  Left(ErrorMessage("One or more input numbers are invalid"))
      case _ :: xs if xs.length < 1                           =>  Left(ErrorMessage("Empty list given"))
      case x :: xs if (x == "divide" && xs.length == 2)       =>  Right(Command.Divide(xs.head.toDouble, xs.reverse.head.toDouble))
      case x :: _ if (x == "divide")                          =>  Left(ErrorMessage("There must be exactly 2 numbers to divide"))
      case x :: xs if (x == "sum")                            =>  Right(Command.Sum(xs.map(_.toDouble)))
      case x :: xs if (x == "min")                            =>  Right(Command.Min(xs.map(_.toDouble)))
      case x :: xs if (x == "max")                            =>  Right(Command.Max(xs.map(_.toDouble)))
      case x :: xs if (x == "average")                        =>  Right(Command.Average(xs.map(_.toDouble)))
      case _                                                  =>  Left(ErrorMessage("Invalid command input"))
    }

  }

  // should return an error (using `Left` channel) in case of division by zero and other
  // invalid operations
  def calculate(x: Command): Either[ErrorMessage, Result] = {
    x match{
      case Command.Divide(dividend, divisor) if divisor == 0  =>  Left(ErrorMessage("Division by zero"))
      case Command.Divide(dividend, divisor)                  =>  Right(CommandResult(x, dividend / divisor))
      case Command.Sum(xs)                                    =>  Right(CommandResult(x, xs.sum))
      case Command.Min(xs)                                    =>  Right(CommandResult(x, xs.min))
      case Command.Max(xs)                                    =>  Right(CommandResult(x, xs.max))
      case Command.Average(xs)                                =>  Right(CommandResult(x, xs.sum / xs.length))
      case _                                                  =>  Left(ErrorMessage("Unimplemented command"))
    }
  }

  def renderResult(x: Result): String = {
    x.asInstanceOf[CommandResult] match {
      case CommandResult(command, value)  => command match{
        case Command.Divide(n1, n2)       =>  f"$n1%.1f divided by $n2%.1f is $value%.1f"
        case Command.Sum(xs)              =>  f"the sum of ${xs.map("%.1f".format(_)).mkString(" ")} is $value%.1f"
        case Command.Min(xs)              =>  f"the minimum of ${xs.map("%.1f".format(_)).mkString(" ")} is $value%.1f"
        case Command.Max(xs)              =>  f"the maximum of ${xs.map("%.1f".format(_)).mkString(" ")} is $value%.1f"
        case Command.Average(xs)          =>  f"the average of ${xs.map("%.1f".format(_)).mkString(" ")} is $value%.1f"
      }
    }
  }

  def process(x: String): String = {
    //import cats.implicits._
    // the import above will enable useful operations on Either-s such as `leftMap`
    // (map over the Left channel) and `merge` (convert `Either[A, A]` into `A`),
    // but you can also avoid using them using pattern matching.

    // implement using a for-comprehension
    val res = for {
      command     <- parseCommand(x)
      result      <- calculate(command)
    } yield result

    res match{
      case Right(r)   =>  renderResult(r)
      case Left(r)    =>  s"Error: ${r.toString()}"
    }


  }

  // This `main` method reads lines from stdin, passes each to `process` and outputs the return value to stdout
  def main(args: Array[String]): Unit = Source.stdin.getLines() map process foreach println
}
