package com.rockthejvm.part3concurrency

import com.rockthejvm.utils.*
import zio.*

object Interruptions extends ZIOAppDefault {

  val zioWithTime =
    (
      ZIO.succeed("starting computation").debugThread *>
      ZIO.sleep(2.seconds) *>
      ZIO.succeed(42).debugThread
    )
      .onInterrupt(ZIO.succeed("I was interrupted!").debugThread)
      // onInterrupt, onDone

  val interruption = for {
    fib <- zioWithTime.fork
    _ <- ZIO.sleep(1.second) *> ZIO.succeed("Interrupting!").debugThread *> fib.interrupt /* <-- is an effect, blocks the calling fiber until the interrupted fiber is done/interrupted */
    _ <- ZIO.succeed("Interruption successful").debugThread
    result <- fib.join
  } yield result

  val interruption_v2 = for {
    fib <- zioWithTime.fork
    _ <- ZIO.sleep(1.second) *> ZIO.succeed("Interrupting!").debugThread *> fib.interruptFork
    _ <- ZIO.succeed("Interruption successful").debugThread
    result <- fib.join
  } yield result

  /*
    Automatic interruption
   */
  // outliving a parent fiber
  val parentEffect =
    ZIO.succeed("spawning fiber").debugThread *>
      // zioWithTime.fork *> // child fiber
      zioWithTime.forkDaemon *> // this fiber will now be a child of the MAIN fiber
      ZIO.sleep(1.second) *>
      ZIO.succeed("parent successful").debugThread // done here

  val testOutlivingParent = for {
    parentEffectFib <- parentEffect.fork
    _ <- ZIO.sleep(3.seconds)
    _ <- parentEffectFib.join
  } yield ()
  // child fibers will be (automatically) interrupted if the parent fiber is completed

  // racing
  val slowEffect = (ZIO.sleep(2.seconds) *> ZIO.succeed("slow").debugThread).onInterrupt(ZIO.succeed("[slow] interrupted").debugThread)
  val fastEffect = (ZIO.sleep(1.second) *> ZIO.succeed("fast").debugThread).onInterrupt(ZIO.succeed("[fast] interrupted").debugThread)
  val aRace = slowEffect.race(fastEffect)
  val testRace = aRace.fork *> ZIO.sleep(3.seconds)

  /**
   * Exercise
   */
  /*
    1 - implement a timeout function
      - if zio is successful before timeout => a successful effect
      - if zio fails before timeout => a failed effect
      - if zio takes longer than timeout => interrupt the effect
   */
  def timeout[R,E,A](zio: ZIO[R,E,A], time: Duration): ZIO[R,E,A] =
    for {
      fib <- zio.fork
      _ <- (ZIO.sleep(time) *> fib.interrupt).fork
      result <- fib.join
    } yield result

  val testTimeout = timeout(
    ZIO.succeed("Starting...").debugThread *> ZIO.sleep(2.seconds) *> ZIO.succeed("I made it!").debugThread,
    1.second
  ).debugThread

  /*
    2 - timeout v2
      - if zio is successful before timeout => a successful effect with Some(a)
      - if zio fails before timeout => a failed effect
      - if zio takes longer than timeout => interrupt the effect, return a successful effect with None
      // hint: foldCauseZIO
   */
  def timeout_v2[R,E,A](zio: ZIO[R,E,A], time: Duration): ZIO[R,E,Option[A]] =
    timeout(zio, time).foldCauseZIO(
      cause => if (cause.isInterrupted) ZIO.succeed(None) else ZIO.failCause(cause),
      value => ZIO.succeed(Some(value))
    )

  val testTimeout_v2 = timeout_v2(
    ZIO.succeed("Starting...").debugThread *> ZIO.sleep(2.seconds) *> ZIO.succeed("I made it!").debugThread,
    1.second
  ).debugThread

  def run = testTimeout_v2
}
