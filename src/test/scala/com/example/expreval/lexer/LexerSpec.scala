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

    "parse a negative number" in {
      assert(getStr(lex("-100")) == "0/-/,1%100%")
      assert(getStr(lex("(-100)")) == "0OP,1/-/,2%100%,5CP")
      assert(getStr(lex("(5+-100)")) == "0OP,1%5%,2/+/,3/-/,4%100%,7CP")
    }

    "parse a multichar operations" in {
      assert(getStr(lex("3>=5")) == "0%3%,1/>=/,3%5%")
      assert(getStr(lex("7+8!=-12")) == "0%7%,1/+/,2%8%,3/!=/,5/-/,6%12%")
      assert(getStr(lex("5+5==6+4")) == "0%5%,1/+/,2%5%,3/==/,5%6%,6/+/,7%4%")
      assert(getStr(lex("var1 > 1 && var2 < -10")) == "0#var1#,5/>/,7%1%,9/&&/,12#var2#,17/</,19/-/,20%10%")
    }

    "parse function calls" in {
      assert(getStr(lex("a(1)")) == "0#a#,1OP,2%1%,3CP")
      assert(getStr(lex("b(2,3)")) == "0#b#,1OP,2%2%,3CM,4%3%,5CP")
      assert(getStr(lex("sum(x*x,y,y)+cos(pi*2+1)")) == "0#sum#,3OP,4#x#,5/*/,6#x#,7CM,8#y#,9CM,10#y#,11CP,12/+/,13#cos#,16OP,17#pi#,19/*/,20%2%,21/+/,22%1%,23CP")
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
      case OPEN_PAREN(pos) => s"${pos}OP"
      case CLOSED_PAREN(pos) => s"${pos}CP"
      case COMMA(pos) => s"${pos}CM"
      case OPERATION(pos, name) => s"$pos/$name/"
      case NAME(pos, name) => s"$pos#$name#"
      case NUMBER(pos, name) => s"$pos%$name%"
      case _ => "Unknown"
    }.mkString(",")
  }


}
