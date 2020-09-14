import java.time.DayOfWeek
import java.util.Scanner

object Main extends App{
  val calc = new Calculator()
  var input = ""
  while(input!="=") {
    input = Console.readLine()
    calc.Process(input=input)
  }
  print(calc.result)
}