package com.example.expreval.lexer

import org.scalatest.WordSpec
import Lexer._
import com.example.expreval.exceptions.SyntaxErrorException
import com.example.expreval.lexer.Token._

class LexerSpec extends WordSpec {
  "lex()" should {
    "parse an empty token" in {
      assert(lex("").isEmpty)
    }

    "parse whitespaces" in {
      assert(lex(" ").isEmpty)
      assert(lex("\t").isEmpty)
      assert(lex(" \t ").isEmpty)
    }

    "parse a single token" in {
      assert(getStr(lex("(")) == "0OP")
      assert(getStr(lex(")")) == "0CP")
      assert(getStr(lex("+")) == "0/+/")
      assert(getStr(lex("A")) == "0#A#")
      assert(getStr(lex("1")) == "0%1%")
    }

    "parse multiple tokens" in {
      assert(getStr(lex(" ( ) ")) == "1OP,3CP")
      assert(getStr(lex("100 + 200")) == "0%100%,4/+/,6%200%")
      assert(getStr(lex("factorial(10)/30-(pi*2 - 1)")) == "0#factorial#,9OP,10%10%,12CP,13///,14%30%,16/-/,17OP,18#pi#,20/*/,21%2%,23/-/,25%1%,26CP")
    }

    "thrown an exception on unknown token" in {
      val ex = intercept[SyntaxErrorException] {
        lex("100 + 200.")
      }
      assert(ex.getMessage.contains("Syntax Error at position 9: Unknown token '.'"))
    }
  }

  private def getStr(tokens: Seq[Token]): String = {
    tokens.map {
      case OPEN_PARAN(pos) => s"${pos}OP"
      case CLOSE_PARAN(pos) => s"${pos}CP"
      case OPERATION(pos, name) => s"$pos/$name/"
      case NAME(pos, name) => s"$pos#$name#"
      case NUMBER(pos, name) => s"$pos%$name%"
      case _ => "Unknown"
    }.mkString(",")
  }


}
