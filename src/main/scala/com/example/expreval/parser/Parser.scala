package com.example.expreval.parser

import com.example.expreval.ast.Expression
import com.example.expreval.exceptions.ExpressionErrorException
import com.example.expreval.lexer.Token._
import com.example.expreval.lexer.{Lexer, Token}

object Parser {

  def parse(expr: String): Expression = {
    val tokens = Lexer.lex(expr)
    val builder = new ExpressionBuilderBigDecimal(expr)
    val parserSM = new ParserSM(builder)

    for (token <- tokens) {
      token match {
        case OPEN_PAREN(pos) => parserSM.onOpenParenthesis(pos)
        case CLOSED_PAREN(pos) => parserSM.onClosedParenthesis(pos)
        case COMMA(pos) => parserSM.onComma(pos)
        case OPERATION(pos, op) => parserSM.onOperaqtion(pos, op)
        case NAME(pos, name) => parserSM.onName(pos, name)
        case NUMBER(pos, name) => parserSM.onName(pos, name)
        case _ => throw new ExpressionErrorException(expr, s"Unknown token $token at ${token.pos}")
      }
    }
    builder.getExpression
  }

}
