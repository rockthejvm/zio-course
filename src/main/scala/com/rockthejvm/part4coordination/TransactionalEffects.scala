package com.rockthejvm.part4coordination

import com.rockthejvm.utils.*
import zio.*
import zio.stm.*

object TransactionalEffects extends ZIOAppDefault {

  // STM = "atomic effect"
  val anSTM: ZSTM[Any, Nothing, Int] = STM.succeed(42)
  val aFailedSTM = STM.fail("something bad")
  val anAttemptSTM: ZSTM[Any, Throwable, Int] = STM.attempt(42 / 0)
  // map, flatMap, for comprehensions

  // type aliases
  val ustm: USTM[Int] = STM.succeed(2)
  val anSTM_v2: STM[Nothing, Int] = STM.succeed(42)

  // STM vs ZIO
  // compose STMs to obtain other STMs
  // evaluation is FULLY ATOMIC
  // "commit"
  val anAtomicEffect: ZIO[Any, Throwable, Int] = anAttemptSTM.commit

  // example
  def transferMoney(sender: Ref[Long], receiver: Ref[Long], amount: Long): ZIO[Any, String, Long] =
    for {
      senderBalance <- sender.get
      _ <- if (senderBalance < amount) ZIO.fail("Transfer failed: Insufficient funds.") else ZIO.unit
      _ <- sender.update(_ - amount)
      _ <- receiver.update(_ + amount)
      newBalance <- sender.get
    } yield newBalance

  def exploitBuggyBank() = for {
    sender <- Ref.make(1000L)
    receiver <- Ref.make(0L)
    fib1 <- transferMoney(sender, receiver, 1000).fork
    fib2 <- transferMoney(sender, receiver, 1000).fork
    _ <- (fib1 zip fib2).join
    _ <- receiver.get.debugThread // should NEVER be > 1000
  } yield ()

  def loop(effect: ZIO[Any, String, Unit], i: Int): ZIO[Any, Nothing, Unit] =
    if (i > 10000)
      ZIO.unit
    else
      effect.ignore *> loop(effect, i + 1)

  // STM implementation
  def transferMoneyTransactional(sender: TRef[Long], receiver: TRef[Long], amount: Long): STM[String, Long] =
    for {
      senderBalance <- sender.get
      _ <- if (senderBalance < amount) STM.fail("Transfer failed: Insufficient funds.") else STM.unit
      _ <- sender.update(_ - amount)
      _ <- receiver.update(_ + amount)
      newBalance <- sender.get
    } yield newBalance

  def cannotExploit() = for {
    sender <- TRef.make(1000L).commit
    receiver <- TRef.make(0L).commit
    fib1 <- transferMoneyTransactional(sender, receiver, 1000).commit.fork
    fib2 <- transferMoneyTransactional(sender, receiver, 1000).commit.fork
    _ <- (fib1 zip fib2).join
    _ <- receiver.get.commit.debugThread // should NEVER be > 1000
  } yield ()

  def run = loop(cannotExploit(), 1)
}
