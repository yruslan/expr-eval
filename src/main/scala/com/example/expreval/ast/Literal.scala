package com.example.expreval.ast

case class Literal(pos: Int, num: BigDecimal) extends Expression
