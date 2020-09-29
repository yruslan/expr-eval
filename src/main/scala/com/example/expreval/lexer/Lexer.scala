package com.example.expreval.lexer

import com.example.expreval.exceptions.SyntaxErrorException
import com.example.expreval.lexer.Token._

import scala.collection.mutable.ArrayBuffer

object Lexer {
  private val WHITESPACE = " \t\n\r"

  // Multichar operations should be listed first so they are first to be considered
  private val OPERATIONS = Array("<=", ">=", "!=", "==", "&&", "||", "!", "+", "-", "*" , "/", "^", "<", ">")

  private val DIGITS = "01234567890"
  private val NAME_START = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_"
  private val NAME_MID = NAME_START + DIGITS

  @throws[SyntaxErrorException]
  def lex(expression: String): Array[Token] = {
    val tokens = new ArrayBuffer[Token]
    var pos = 0

    def findChars(startingChars: String, midChars: String): String = {
      var pos2 = pos

      if (startingChars.contains(expression(pos2))) {
        pos2 += 1
        while (pos2 < expression.length && midChars.contains(expression.charAt(pos2))) {
          pos2 += 1
        }
      }

      if (pos2 == pos) {
        ""
      } else {
        val s = expression.substring(pos, pos2)
        pos = pos2
        s
      }
    }

    def findSingleCharToken(): Boolean = {
      val c = expression.charAt(pos)

      val tokenOpt = c match {
        case '(' => Some(OPEN_PAREN(pos))
        case ')' => Some(CLOSED_PAREN(pos))
        case ',' => Some(COMMA(pos))
        case _ => None
      }

      tokenOpt match {
        case Some(token) =>
          pos += 1
          tokens += token
          true
        case None =>
          false
      }
    }

    def findWhiteSpace(): Boolean = {
      val p = pos
      val s = findChars(WHITESPACE, WHITESPACE)

      if (s.isEmpty) {
        false
      } else {
        true
      }
    }

    def findNumber(): Boolean = {
      val p = pos
      val s = findChars(DIGITS, DIGITS)

      if (s.isEmpty) {
        false
      } else {
        tokens += NUMBER(p, s)
        true
      }
    }

    def findName(): Boolean = {
      val p = pos
      val s = findChars(NAME_START, NAME_MID)

      if (s.isEmpty) {
        false
      } else {
        tokens += NAME(p, s)
        true
      }
    }

    def findOperations(): Boolean = {
      val s = expression.substring(pos)

      val optOp = OPERATIONS.find(op => s.startsWith(op))

      optOp match {
        case Some(op) =>
          tokens += OPERATION(pos, op)
          pos += op.length
          true
        case None =>
          false
      }
    }

    while (pos < expression.length) {
      val ok = findSingleCharToken() || findWhiteSpace() || findOperations() || findNumber() || findName()

      if (!ok) {
        throw new SyntaxErrorException(pos, s"Unknown token '${expression.charAt(pos)}'")
      }
    }

    tokens.toArray
  }
}
