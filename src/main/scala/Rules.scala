class Rules {
  def isDigit(text:String): Boolean ={
    var digit: Boolean = true
    for (i<-text) if (!i.isDigit) digit=false
    digit
  }
  def isOperation(text:String): Boolean={
    val operations = Array("+", "-", "*", "/")
    if (operations.contains(text)) true else false
  }
  def isEqual(text:String): Boolean = if (text=="=") true else false
}
