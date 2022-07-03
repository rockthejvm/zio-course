package com.rockthejvm.part3concurrency

import com.rockthejvm.utils.*
import zio.*

object MasteringInterruptions extends ZIOAppDefault {

  // interruptions:
  // fib.interrupt
  // ZIO.race, ZIO.zipPar, ZIO.collectAllPar
  // outliving parent fiber

  // manual interruption
  val aManuallyInterruptedZIO = ZIO.succeed("computing...").debugThread *> ZIO.interrupt *> ZIO.succeed(42).debugThread

  // finalizer
  val effectWithInterruptionFinalizer = aManuallyInterruptedZIO.onInterrupt(ZIO.succeed("I was interrupted!").debugThread)

  // uninterruptible
  // payment flow to NOT be interrupted
  val fussyPaymentSystem = (
    ZIO.succeed("payment running, don't cancel me...").debugThread *>
    ZIO.sleep(1.second) *> // the actual payment
    ZIO.succeed("payment completed").debugThread
  ).onInterrupt(ZIO.succeed("MEGA CANCEL OF DOOM!").debugThread) // don't want this triggered

  val cancellationOfDoom = for {
    fib <- fussyPaymentSystem.fork
    _ <- ZIO.sleep(500.millis) *> fib.interrupt
    _ <- fib.join
  } yield ()

  // ZIO.uninterruptible
  val atomicPayment = ZIO.uninterruptible(fussyPaymentSystem) // make a ZIO atomic
  val atomicPayment_v2 = fussyPaymentSystem.uninterruptible // same

  val noCancellationOfDoom = for {
    fib <- atomicPayment.fork
    _ <- ZIO.sleep(500.millis) *> fib.interrupt
    _ <- fib.join
  } yield ()

  // interruptibility is regional
  val zio1 = ZIO.succeed(1)
  val zio2 = ZIO.succeed(2)
  val zio3 = ZIO.succeed(3)
  val zioComposed = (zio1 *> zio2 *> zio3).uninterruptible // ALL the zios are uninterruptible
  val zioComposed2 = (zio1 *> zio2.interruptible *> zio3).uninterruptible // inner scopes override outer scopes

  // uninterruptibleMask
  /*
   example: an authentication service
   - input password, can be interrupted, because otherwise it might block the fiber indefinitely
   - verify password, which cannot be interrupted once it's triggered
   */
  val inputPassword = for {
    _ <- ZIO.succeed("Input password:").debugThread
    _ <- ZIO.succeed("(typing password)").debugThread
    _ <- ZIO.sleep(2.seconds)
    pass <- ZIO.succeed("RockTheJVM1!")
  } yield pass

  def verifyPassword(pw: String) = for {
    _ <- ZIO.succeed("verifying...").debugThread
    _ <- ZIO.sleep(2.seconds)
    result <- ZIO.succeed(pw == "RockTheJVM1!")
  } yield result

  val authFlow = ZIO.uninterruptibleMask { restore =>
    // EVERYTHING is uninterruptible...
    for {
      pw <- restore(inputPassword) /* <- ... except this thing */.onInterrupt(ZIO.succeed("Authentication timed out. Try again later.").debugThread)
            // ^^ restores the interruptibility flag of this ZIO at the time of the call
      verification <- verifyPassword(pw)
      _ <-  if (verification) ZIO.succeed("Authentication successful.").debugThread
            else ZIO.succeed("Authentication failed.").debugThread
    } yield ()
  }

  val authProgram = for {
    authFib <- authFlow.fork
    _ <- ZIO.sleep(3.seconds) *> ZIO.succeed("Attempting to cancel authentication...").debugThread *> authFib.interrupt
    _ <- authFib.join
  } yield ()

  /**
   * Exercises
   */
  // what will these effects do? (without running them in your app)
  val cancelBeforeMol = ZIO.interrupt *> ZIO.succeed(42).debugThread
  val uncancelBeforeMol = ZIO.uninterruptible(ZIO.interrupt *> ZIO.succeed(42).debugThread)
  // neither will print anything!

  // 2
  val authProgram_v2 = for {
    authFib <- ZIO.uninterruptibleMask(_ => authFlow).fork
    _ <- ZIO.sleep(1.seconds) *> ZIO.succeed("Attempting to cancel authentication...").debugThread *> authFib.interrupt
    _ <- authFib.join
  } yield ()
  /*
    Uninterruptible calls are masks which suppress cancellation. Restorer opens "gaps" in the uninterruptible region.
    If you wrap an entire structure with another .uninterruptible/.uninterruptibleMask, you'll cover those gaps too.
    For this example, the program will cover all interruptible gaps, so the interruption signals will be ignored.
   */

  // 3
  val threeStepProgram = {
    val sequence = ZIO.uninterruptibleMask { restore =>
      for {
        _ <- restore(ZIO.succeed("interruptible").debugThread *> ZIO.sleep(1.second))
        _ <- ZIO.succeed("uninterruptible").debugThread *> ZIO.sleep(1.second)
        _ <- restore(ZIO.succeed("interruptible 2").debugThread *> ZIO.sleep(1.second))
      } yield ()
    }

    for {
      fib <- sequence.fork
      _ <- ZIO.sleep(1500.millis) *> ZIO.succeed("INTERRUPTING!").debugThread *> fib.interrupt
      _ <- fib.join
    } yield ()
  }

  def run = threeStepProgram
}
