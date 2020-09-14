trait CalculatorState
case object AccumulateDigit extends CalculatorState
case object Operation extends CalculatorState
case object Result extends CalculatorState

class Calculator {
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
        else if(rules.isDigit(text)) accumulateDigit(text, true)
        else if(rules.isEqual(text)) result(text, true)
      }
    }

    def calculate(): Unit ={
      try {
        if (operation == "+") {
          result = (result.toInt + tempNumber.toInt).toString
          print(result, tempNumber)
        }
        if (operation == "-") result = (result.toInt - tempNumber.toInt).toString
        if (operation == "*") result = (result.toInt * tempNumber.toInt).toString
        if (operation == "/") result = (result.toInt / tempNumber.toInt).toString
      }
      catch {
        case _: Throwable => println("Please enter char by char")
      }
    }

    def operation(text: String, ok:Boolean): Unit ={
      if (ok){
        state = Operation
        if (operation.length>0) calculate()
        else {
          result = tempNumber
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
