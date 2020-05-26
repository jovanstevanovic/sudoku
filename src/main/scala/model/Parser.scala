package model

object Parser {
  // Class fields
  var operations : Array[Operation] = new Array[Operation](0)

  // Inner class for storing operations.
  class Operation(val functionName : String, val op1 : Int = 0, val op2 : Int = 0, val op3 : Int = 0) {}

  // Parsing methods
  def parseLine(line : String): Array[Operation] = {
    operations = new Array[Operation](0)
    if(line.contains(">>")) {
      parseCompositeOperation(line)
    } else {
      parseSequenceOperations(line)
    }
    operations
  }

  private def parseCompositeOperation(line : String): Unit = {
    val listOfFunctions = line.split( ">>")
    for(i <- 0 until listOfFunctions.length - 1) {
      parseOperands(listOfFunctions(i), listOfFunctions(listOfFunctions.length - 1))
    }
  }

  private def parseSequenceOperations(line : String): Unit = {
    val listOfFunctions = line.split("&")
    for(function <- listOfFunctions) {
      val functionName = function.split("=>")(0)
      val argumentList = function.split("=>")(1)
      parseOperands(functionName, argumentList)
    }
  }

  private def parseOperands(functionName : String, operandsString : String): Unit = {
    val listOfOperands = operandsString.split(",")
    listOfOperands.length match {
      case 0 =>
        operations :+=  new Operation(functionName)
      case 1 =>
        operations :+= new Operation(functionName, listOfOperands(0).toInt)
      case 2 =>
        operations :+= new Operation(functionName, listOfOperands(0).toInt, listOfOperands(1).toInt)
      case 3 =>
        operations :+= new Operation(functionName, listOfOperands(0).toInt, listOfOperands(1).toInt, listOfOperands(2).toInt)
    }
  }
}
