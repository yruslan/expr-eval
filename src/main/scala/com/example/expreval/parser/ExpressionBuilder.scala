package com.example.expreval.parser

import com.example.expreval.ast.Expression

trait ExpressionBuilder {
  def getExpression: Expression

  def setOperation(c: Char): Unit

  def setName(pos: Int, name: String): Unit

  def setVariable(): Unit
  def setFunction(): Unit

  def setNumber(pos: Int, num: String): Unit

  def setNextArgument(pos: Int): Unit

  def openParenthesis(): Unit
  def closedParenthesis(): Unit
}
