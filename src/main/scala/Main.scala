import java.time.DayOfWeek
import java.util.Scanner
trait CalculatorState
case object AccumulateDigit extends CalculatorState
case object Operation extends CalculatorState
case object Result extends CalculatorState


class Rules(){
  def isDigit(text:String): Boolean ={
    val digits = Array("0","1","2","3","4","5","6","7","8","9")
    print(digits.contains(text))
    if (digits.contains(text)) true else false
  }
  def isOperation(text:String): Boolean={
    val operations = Array("+", "-", "*", "/")
    print(operations.contains(text))
    if (operations.contains(text)) true else false
  }
  def isEqual(text:String): Boolean ={
    if (text=="=") true else false
  }
}

class Calculator() {
  var tempNumber = ""
  var operation = ""
  var result = ""
  var state: CalculatorState = AccumulateDigit
  var rules = new Rules
  val operations = Array("+", "-", "*", "/")

  def Process(input: String=""): Unit ={
    state match {
      case AccumulateDigit => accumulateDigit(input, false)
      case Operation => operation(input, false)
      case Result => result(input, false)
    }
  }
  def accumulateDigit(text:String, ok: Boolean): Unit ={
    if (ok){
      state = AccumulateDigit
      tempNumber+=text
    };
    else{
      if(rules.isOperation(text)) operation(text, true)
      if(rules.isDigit(text)) accumulateDigit(text, true)
      if(rules.isEqual(text)) result(text, true)
    }
  }

  def calculate(): Unit ={
    if (operation=="+") result=(result.toInt+tempNumber.toInt).toString
    if (operation=="-") result=(result.toInt-tempNumber.toInt).toString
    if (operation=="*") result=(result.toInt*tempNumber.toInt).toString
    if (operation=="/") result=(result.toInt/tempNumber.toInt).toString
  }


  def operation(text: String, ok:Boolean): Unit ={
    if (ok){
      state = Operation
      if (operation.length>0) calculate()
      else {
        result = tempNumber
        print("result", result)
        tempNumber=""
        operation=text
      }
    }
    else{
      if (rules.isDigit(text))
        accumulateDigit(text, true)
    }
  }

  def result(input: String, ok: Boolean): Unit ={
    if (ok){
      state = Result
      calculate()
    }
    else{
      if(rules.isDigit(input)) accumulateDigit(input, true)
    }
  }

}



object Main extends App{
  val calc = new Calculator()
  var input = ""
  while(input!="=") {
    input = Console.readLine()
    calc.Process(input=input)
  }
  print(calc.result)

}