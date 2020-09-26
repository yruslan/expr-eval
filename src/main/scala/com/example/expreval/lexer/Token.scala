package com.example.expreval.lexer

sealed trait Token {
  def pos: Int
}

object Token {
  case class OPEN_PARAN (pos: Int) extends Token {
    override def toString: String = "("
  }

  case class CLOSE_PARAN (pos: Int) extends Token {
    override def toString: String = ")"
  }

  case class OPERATION (pos: Int, name: Char) extends Token {
    override def toString: String = s"$name"
  }

  case class NAME(pos: Int, name: String) extends Token {
    override def toString: String = s"$name"
  }

  case class NUMBER (pos: Int, name: String) extends Token {
    override def toString: String = s"$$name"
  }

}
