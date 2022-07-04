package com.rockthejvm.part3concurrency

import com.rockthejvm.utils.*
import zio.*

import java.util.concurrent.{ExecutorService, Executors}
import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success, Try}

object AsynchronousEffects extends ZIOAppDefault {

  // CALLBACK-based
  // asynchronous
  object LoginService {
    case class AuthError(message: String)
    case class UserProfile(email: String, name: String)

    // thread pool
    val executor = Executors.newFixedThreadPool(8)

    // "database"
    val passwd = Map(
      "daniel@rockthejvm.com" -> "RockTheJVM1!"
    )

    // the profile data
    val database = Map(
      "daniel@rockthejvm.com" -> "Daniel"
    )

    def login(email: String, password: String)(onSuccess: UserProfile => Unit, onFailure: AuthError => Unit): Unit =
      executor.execute { () =>
        println(s"[${Thread.currentThread().getName}] Attempting login for $email")
        passwd.get(email) match {
          case Some(`password`) =>
            onSuccess(UserProfile(email, database(email)))
          case Some(_) =>
            onFailure(AuthError("Incorrect password."))
          case None =>
            onFailure(AuthError(s"User $email doesn't exist. Please sign up."))
        }
      }
  }

  def loginAsZIO(id: String, pw: String): ZIO[Any, LoginService.AuthError, LoginService.UserProfile] =
    ZIO.async[Any, LoginService.AuthError, LoginService.UserProfile] { cb => // callback object created by ZIO
      LoginService.login(id, pw)(
        profile => cb(ZIO.succeed(profile)), // notify the ZIO fiber to complete the ZIO with a success
        error => cb(ZIO.fail(error)) // same, with a failure
      )
    }

  val loginProgram = for {
    email <- Console.readLine("Email: ")
    pass <- Console.readLine("Password: ")
    profile <- loginAsZIO(email, pass).debugThread
    _ <- Console.printLine(s"Welcome to Rock the JVM, ${profile.name}")
  } yield ()

  /**
   * Exercises
   */
  // 1 - lift a computation running on some (external) thread to a ZIO
  // hint: invoke the cb when the computation is complete
  // hint 2: don't wrap the computation into a ZIO
  def external2ZIO[A](computation: () => A)(executor: ExecutorService): Task[A] =
    ZIO.async[Any, Throwable, A] { cb =>
      executor.execute { () =>
        try {
          val result = computation()
          cb(ZIO.succeed(result))
        } catch {
          case e: Throwable => cb(ZIO.fail(e))
        }
      }
    }

  val demoExternal2ZIO = {
    val executor = Executors.newFixedThreadPool(8)
    val zio: Task[Int] = external2ZIO { () =>
      println(s"[${Thread.currentThread().getName}] computing the meaning of life on some thread")
      Thread.sleep(1000)
      42
    } (executor)

    zio.debugThread.unit
  }

  // 2 - lift a Future to a ZIO
  // hint: invoke cb when the Future completes
  def future2ZIO[A](future: => Future[A])(using ec: ExecutionContext): Task[A] =
    ZIO.async[Any, Throwable, A] { cb =>
      future.onComplete {
        case Success(value) => cb(ZIO.succeed(value))
        case Failure(ex) => cb(ZIO.fail(ex))
      }
    }

  lazy val demoFuture2ZIO = {
    val executor = Executors.newFixedThreadPool(8)
    given ec: ExecutionContext = ExecutionContext.fromExecutorService(executor)
    val mol: Task[Int] = future2ZIO(Future {
      println(s"[${Thread.currentThread().getName}] computing the meaning of life on some thread")
      Thread.sleep(1000)
      42
    })

    mol.debugThread.unit
  }

  // 3 - implement a never-ending ZIO
  def neverEndingZIO[A]: UIO[A] =
    ZIO.async(_ => ())

  val never = ZIO.never

  def run = ZIO.succeed("Computing...").debugThread *> neverEndingZIO[Int] *> ZIO.succeed("Completed.").debugThread
}
