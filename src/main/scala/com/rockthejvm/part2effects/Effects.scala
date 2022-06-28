package com.rockthejvm.part2effects

import scala.concurrent.Future

object Effects {

  // functional programming
  // EXPRESSIONS
  def combine(a: Int, b: Int): Int = a + b

  // local reasoning = type signature describes the kind of computation that will be performed
  // referential transparency = ability to replace an expression with the value that it evaluates to
  val five = combine(2, 3)
  val five_v2 = 2 + 3
  val five_v3 = 5

  // not all expressions are RT
  // example 1: printing
  val resultOfPrinting: Unit = println("Learning ZIO")
  val resultOfPrinting_v2: Unit = () // not the same

  // example 2: changing a variable
  var anInt = 0
  val changingInt: Unit = (anInt = 42) // side effect
  val changingInt_v2: Unit = () // not the same program

  // side effects are inevitable

  /*
    Effect properties:
    - the type signature describes what KIND of computation it will perform
    - the type signature describes the type of VALUE that it will produce
    - if side effects are required, construction must be separate from the EXECUTION
   */

  /*
    Example: Options = possibly absent values
    - type signature describes the kind of computation = a possibly absent value
    - type signature says that the computation returns an A, if the computation does produce something
    - no side effects are needed

    => Option is an effect
   */
  val anOption: Option[Int] = Option(42)

  /*
    Example 2: Future
    - describes an asynchronous computation
    - produces a value of type A, if it finishes and it's successful
    - side effects are required, construction is NOT SEPARATE from execution

    => Future is NOT an effect
   */
  import scala.concurrent.ExecutionContext.Implicits.global
  val aFuture: Future[Int] = Future(42)

  /*
    Example 3: MyIO
    - describes a computation which might perform side effects
    - produces a value of type A if the computation is successful
    - side effects are required, construction IS SEPARATE from execution

    My IO IS AN EFFECT!
   */
  case class MyIO[A](unsafeRun: () => A) {
    def map[B](f: A => B): MyIO[B] =
      MyIO(() => f(unsafeRun()))

    def flatMap[B](f: A => MyIO[B]): MyIO[B] =
      MyIO(() => f(unsafeRun()).unsafeRun())
  }

  val anIOWithSideEffects: MyIO[Int] = MyIO(() => {
    println("producing effect")
    42
  })


  def main(args: Array[String]): Unit = {
    anIOWithSideEffects.unsafeRun()
  }
}
