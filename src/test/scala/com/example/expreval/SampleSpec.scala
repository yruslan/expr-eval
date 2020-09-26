package com.example.expreval

import org.scalatest.{FlatSpec, Matchers}

class SampleSpec extends FlatSpec with Matchers {

  "simpleTest" should "just work" in {
    val v = 2 + 2
    v shouldEqual 4
    println ("Simple Test successful")
  }

}
