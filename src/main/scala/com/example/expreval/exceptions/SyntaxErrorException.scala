package com.example.expreval.exceptions

class SyntaxErrorException(pos: Int, msg: String, cause: Throwable = null)
  extends Exception(s"Syntax Error at position $pos: $msg", cause)
