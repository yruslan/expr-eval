package com.example.expreval.exceptions

class ExpressionErrorException(expr: String, msg: String, cause: Throwable = null)
  extends Exception(s"Error: $msg in expression: '$expr'", cause)
