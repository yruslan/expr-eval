package com.example.expreval.parser

import com.example.expreval.ast.{Expression, Func, Literal, Variable}
import com.example.expreval.exceptions.{ExpressionErrorException, SyntaxErrorException}

import scala.collection.mutable.ListBuffer
import scala.util.{Failure, Success, Try}

class ExpressionBuilderBigDecimal(expr: String) {

  private val arguments = new ListBuffer[Expression]

  private val operations = new ListBuffer[Expression]

  private var operation = ""
  private var name = ""
  private var num = BigDecimal(0)
  private var arg: Expression = _
  private var pos: Int = 0

  def getExpression: Expression = {
    if (arguments.isEmpty) {
      throw new ExpressionErrorException(expr, "Empty expressions are not supported")
    }
    if (arguments.lengthCompare(1) > 0) {
      throw new ExpressionErrorException(expr, "The expression returned more than one result")
    }
    arguments.head
  }

  def setOperation(op: String): Unit = operation = op

  def setName(pos: Int, name: String): Unit = {
    this.pos = pos
    this.name = name
  }

  def setVariable(): Unit = {
    arg = Variable(pos, name)
    arguments.prepend(arg)
  }

  def setFunction(): Unit = {
    arg = Func(pos, name, Nil)
  }

  def setNumber(pos: Int, s: String): Unit = {
    val n = Try(BigDecimal(s)) match {
      case Success(value) => value
      case Failure(exception) => throw new SyntaxErrorException(pos, s"Cannot parse the number '$s'")
    }
    arg = Literal(pos, n)
    arguments.prepend(arg)
  }

  def error(pos: Int): Unit = {
    throw new SyntaxErrorException(pos, s"Unexpected token.")
  }

  def setNextArgument(pos: Int): Unit = {}

  def openParenthesis(): Unit = {}
  def closedParenthesis(): Unit = {}
}
