package com.rockthejvm.part5testing

import zio._
import zio.test._
import zio.test.TestAspect._

object SimpleDependencySpec extends ZIOSpecDefault {
  def spec = test("simple dependency") {
    val aZIO: ZIO[Int, Nothing, Int] = ZIO.succeed(42)
    assertZIO(aZIO)(Assertion.equalTo(42))
  }.provide(ZLayer.succeed(10))
}

// example: a user survey application processing user data, fetching data from a database
object BusinessLogicSpec extends ZIOSpecDefault {

  // "dependency" (sits elsewhere)
  abstract class Database[K,V] {
    def get(key: K): Task[V]
    def put(key: K, value: V): Task[Unit]
  }
  object Database {
    def create(url: String): UIO[Database[String, String]] = ??? // the REAL thing
  }

  // "business" logic under test
  def normalizeUsername(name: String): UIO[String] = ZIO.succeed(name.toUpperCase())

  val mockedDatabase = ZIO.succeed(new Database[String, String] {
    import scala.collection.mutable
    val map = mutable.Map[String, String]()

    override def get(key: String) = ZIO.attempt(map(key))
    override def put(key: String, value: String) = ZIO.succeed(map += (key -> value))
  })

  // testing
  def spec = suite("A user survey application should...")(
    test("normalize user names") {
      val surveyPreliminaryLogic = for {
        db <- ZIO.service[Database[String, String]]
        _ <- db.put("123", "Daniel")
        username <- db.get("123")
        normalized <- normalizeUsername(username)
      } yield normalized

      assertZIO(surveyPreliminaryLogic)(Assertion.equalTo("DANIEL"))
    }
  ).provide(ZLayer.fromZIO(mockedDatabase))
}

/*
  built-in test services
  - console
  - random
  - clock
  - system
 */
object DummyConsoleApplication {
  def welcomeUser(): Task[Unit] = for {
    _ <- Console.printLine("Please enter your name...")
    name <- Console.readLine("")
    _ <- Console.printLine(s"Welcome, $name!")
  } yield ()
}

object BuiltInTestServicesSpec extends ZIOSpecDefault {
  def spec = suite("Checking built-in test services")(
    test("ZIO console application") {
      val logicUnderTest: Task[Vector[String]] = for {
        _ <- TestConsole.feedLines("Daniel")
        _ <- DummyConsoleApplication.welcomeUser()
        output <- TestConsole.output
      } yield output.map(_.trim)

      assertZIO(logicUnderTest)(Assertion.hasSameElements(List("Please enter your name...", "", "Welcome, Daniel!")))
    },
    test("ZIO clock") {
      val parallelEffect = for {
        fiber <- ZIO.sleep(5.minutes).timeout(1.minute).fork
        _ <- TestClock.adjust(1.minute)
        result <- fiber.join
      } yield result

      assertZIO(parallelEffect)(Assertion.isNone)
    },
    test("ZIO Random") {
      val effect = for {
        _ <- TestRandom.feedInts(3,4,1,2)
        value <- Random.nextInt
      } yield value

      assertZIO(effect)(Assertion.equalTo(3))
    }
  )
}

/*
  Test aspects
 */
object AspectsSpec extends ZIOSpecDefault {
  def computeMeaningOfLife: UIO[Int] =
    ZIO.sleep(2.seconds) *> ZIO.succeed(42)

  def spec = suite("Testing Aspects")(
    test("timeout aspect") {
      val effect = for {
        molFib <- computeMeaningOfLife.fork
        _ <- TestClock.adjust(3.seconds)
        v <- molFib.join
      } yield v

      assertZIO(effect)(Assertion.equalTo(42))
    } @@ timeout(10.millis) @@ timed
    /*
      Aspects:
      - timeout(duration)
      - eventually - retries until successful
      - nonFlaky(n) - repeats n times, stops at first failure
      - repeats(n) - same
      - retries(n) - retries n times, stops at first success
      - debug - prints everything in the console
      - silent - prints nothing
      - diagnose(duration)
      - parallel/sequential (aspects of a SUITE)
      - ignore
      - success - fail all ignored tests
      - timed - measure execution time
      - before/beforeAll + after/afterAll
     */
  )
}
