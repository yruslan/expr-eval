package com.example.expreval.ast

case class Func(pos: Int, name: String, arguments: Seq[Expression]) extends Expression