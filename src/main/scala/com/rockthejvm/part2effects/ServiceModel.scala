package com.rockthejvm.part2effects

import zio._

object ServiceModel {

  // app to subscribe users to newsletter
  case class User(name: String, email: String)

  class UserSubscription(emailService: EmailService, userDatabase: UserDatabase) {
    def subscribeUser(user: User): Task[Unit] =
      for {
        _ <- emailService.email(user)
        _ <- userDatabase.insert(user)
      } yield ()
  }

  object UserSubscription {
    def create(emailService: EmailService, userDatabase: UserDatabase) =
      new UserSubscription(emailService, userDatabase)

    val live: ZLayer[EmailService & UserDatabase, Nothing, UserSubscription] =
      ZLayer.fromFunction(new UserSubscription(_, _))
  }

  class EmailService {
    def email(user: User): Task[Unit] =
      ZIO.succeed(println(s"You've just been subscribed to Rock the JVM. Welcome, ${user.name}!"))
  }

  object EmailService {
    def create(): EmailService = new EmailService
    val live: ZLayer[Any, Nothing, EmailService] =
      ZLayer.succeed(new EmailService)
  }

  class UserDatabase(connectionPool: ConnectionPool) {
    def insert(user: User): Task[Unit] = for {
      conn <- connectionPool.get
      _ <- conn.runQuery(s"insert into subscribers(name, email) values (${user.name}, ${user.email})")
    } yield ()
  }

  object UserDatabase {
    def create(connectionPool: ConnectionPool) =
      new UserDatabase(connectionPool)
    val live: ZLayer[ConnectionPool, Nothing, UserDatabase] =
      ZLayer.fromFunction(new UserDatabase(_))
  }

  class ConnectionPool(nConnections: Int) {
    def get: Task[Connection] =
      ZIO.succeed(println("Acquired connection")) *> ZIO.succeed(Connection())
  }

  object ConnectionPool {
    def create(nConnections: Int) =
      new ConnectionPool(nConnections)
    def live(nConnections: Int): ZLayer[Any, Nothing, ConnectionPool] =
      ZLayer.succeed(new ConnectionPool(nConnections))
  }

  case class Connection() {
    def runQuery(query: String): Task[Unit] =
      ZIO.succeed(println(s"Executing query: $query"))
  }
}
