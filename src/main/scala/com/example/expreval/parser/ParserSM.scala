package com.example.expreval.parser

import com.example.expreval.ast.Expression
import com.example.expreval.lexer.Token

class ParserSM(expressionBuilder: ExpressionBuilderBigDecimal) {
  def onOpenParenthesis(pos: Int): Unit = {
    handleEvent(pos, OPEN_PAREN)
  }

  def onClosedParenthesis(pos: Int): Unit = {
    handleEvent(pos, CLOSED_PAREN)
  }

  def onComma(pos: Int): Unit = {
    handleEvent(pos, COMMA)
  }

  def onOperaqtion(pos: Int, operation: String): Unit = {
    this.operation = operation
    handleEvent(pos, OPERATION)
  }

  def onName(pos: Int, s: String): Unit = {
    expressionBuilder.setName(pos, s)
    handleEvent(pos, NAME)
  }

  def onNumber(pos: Int, s: String): Unit = {
    expressionBuilder.setNumber(pos, s)
    handleEvent(pos, NUMBER)
  }

  def getExpression: Expression = expressionBuilder.getExpression

  // States
  val STATE_EXPRESSION = 0
  val STATE_VAR_FUNCTION = 1
  val STATE_OPERATION_OR_CLOSED_P = 2

  // Events
  val OPEN_PAREN = 0
  val CLOSED_PAREN = 1
  val COMMA = 2
  val OPERATION = 3
  val NAME = 4
  val NUMBER = 5

  private var state = STATE_EXPRESSION
  private var position = 0
  private var operation = ""

  case class Transition(state: Int, event: Int, nextState: Int, action: Token => Unit)

  val transitions = Seq(
    Transition(STATE_EXPRESSION, OPEN_PAREN,   STATE_EXPRESSION,              token => expressionBuilder.openParenthesis()),
    Transition(STATE_EXPRESSION, CLOSED_PAREN, STATE_EXPRESSION,              token => expressionBuilder.error(position)),
    Transition(STATE_EXPRESSION, COMMA,        STATE_EXPRESSION,              token => expressionBuilder.error(position)),
    Transition(STATE_EXPRESSION, OPERATION,    STATE_EXPRESSION,              token => expressionBuilder.error(position)),
    Transition(STATE_EXPRESSION, NAME,         STATE_VAR_FUNCTION,            _ => ()),
    Transition(STATE_EXPRESSION, NUMBER,       STATE_OPERATION_OR_CLOSED_P,   _ => ()),

    Transition(STATE_VAR_FUNCTION, OPEN_PAREN, STATE_EXPRESSION,              token => expressionBuilder.setFunction()),
    Transition(STATE_VAR_FUNCTION, CLOSED_PAREN, STATE_OPERATION_OR_CLOSED_P, token => {expressionBuilder.setVariable(); expressionBuilder.closedParenthesis()} ),
    Transition(STATE_VAR_FUNCTION, COMMA,     STATE_EXPRESSION,               token => {expressionBuilder.setVariable(); expressionBuilder.setNextArgument(position)}),
    Transition(STATE_VAR_FUNCTION, OPERATION, STATE_EXPRESSION,               token => {expressionBuilder.setVariable(); expressionBuilder.setOperation(operation) } ),
    Transition(STATE_VAR_FUNCTION, NAME,      STATE_EXPRESSION,               token => expressionBuilder.error(position)),
    Transition(STATE_VAR_FUNCTION, NUMBER,    STATE_EXPRESSION,               token => expressionBuilder.error(position)),

    Transition(STATE_OPERATION_OR_CLOSED_P, OPEN_PAREN, STATE_EXPRESSION,              token => expressionBuilder.error(position)),
    Transition(STATE_OPERATION_OR_CLOSED_P, CLOSED_PAREN, STATE_OPERATION_OR_CLOSED_P, token => expressionBuilder.closedParenthesis()),
    Transition(STATE_OPERATION_OR_CLOSED_P, COMMA,     STATE_EXPRESSION,               token => expressionBuilder.setNextArgument(position)),
    Transition(STATE_OPERATION_OR_CLOSED_P, OPERATION, STATE_EXPRESSION,               token => expressionBuilder.setOperation(operation)),
    Transition(STATE_OPERATION_OR_CLOSED_P, NAME,      STATE_EXPRESSION,               token => expressionBuilder.error(position)),
    Transition(STATE_OPERATION_OR_CLOSED_P, NUMBER,    STATE_EXPRESSION,               token => expressionBuilder.error(position))
  )

  private def handleEvent(pos: Int, event: Int): Unit = {
    position = pos

    val transition = transitions.find(t => t.state == state && t.event == event)

    transition match {
      case Some(t) =>
        state = t.nextState
        val _ = t.action
      case None => throw new IllegalStateException(s"Unexpected parsing error at pos $pos")
    }
  }

}
