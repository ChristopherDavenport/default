package io.chrisdavenport.default

import org.specs2._

object MainSpec extends mutable.Specification {

  "Default" should {
    "return zero for Int" in {
      Default[Int].default should_=== 0
    }
    "return zero for BigInt" in {
      Default[BigInt].default should_=== BigInt(0)
    }
    "return empty string for String" in {
      default[String] should_=== ""
    }
  }

}