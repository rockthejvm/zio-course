package com.rockthejvm.part5testing

import zio._
import zio.test._

case class Person(name: String, age: Int) {
  def spellName: String = name.toUpperCase()
  def saySomething: UIO[String] = ZIO.succeed(s"Hi, I'm $name")
}

object MyTestSpec extends ZIOSpecDefault {
  // fundamental method
  def spec = test("First Test") {
    val person = Person("Daniel", 99)

    // an assertion
    assert(person.spellName)(Assertion.equalTo("DANIEL"))
    // same
    assertTrue(person.spellName == "DANIEL")
  }
}

object MyFirstEffectTestSpec extends ZIOSpecDefault {
  def spec = test("First Effect Test") {
    val person = Person("Daniel", 101)
    assertZIO(person.saySomething)(Assertion.equalTo("Hi, I'm Daniel"))
    assertZIO(person.saySomething)(Assertion.assertion("should be a greeting")(gr => gr == "Hi, I'm Daniel"))
    // doesn't work with assertTrue
    /*
      Assertion examples:
      - Assertion.assertion => tests any truth value = the most general assertion
      - Assertion.equalTo => tests for equality
      - Assertion.fails/failsCause => expects the effect to fail with the EXACT failure/cause you specify
      - Assertion.dies => expects the effect to die with a Throwable, can run an assertion on that Throwable
      - Assertion.isInterrupted => validates an interruption
      - Specialized
        - isLeft/isRight for Either
        - isSome/isNone for Option
        - isSuccess/isFailure for Try
        - isEmpty/nonEmpty, contains, has* for iterables
        - isEmptyString/nonEmptyString, startsWithString, matchesRegex for Strings
     */
  }
}

object ASuiteSpec extends ZIOSpecDefault {
  def spec = suite("Full suite of tests")(
    // pass multiple tests as arguments
    test("simple test") {
      assertTrue(1 + 3 == 4)
    },
    test("a second test") {
      assertZIO(ZIO.succeed("Scala"))(Assertion.hasSizeString(Assertion.equalTo(5)) && Assertion.startsWithString("S"))
    },
    // sub-suites
    suite("a nested suite")(
      test("a nested test") {
        assert(List(1,2,3))(Assertion.isNonEmpty && Assertion.hasSameElements(List(3,1,2)))
      },
      test("another nested test") {
        assert(List(1,2,3).headOption)(Assertion.equalTo(Some(1)))
      },
      test("a failed nested test") {
        assertTrue(1 + 1 == 100)
      }
    )
  )
}