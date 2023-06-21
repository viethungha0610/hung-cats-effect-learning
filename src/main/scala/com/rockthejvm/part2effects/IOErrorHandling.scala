package com.rockthejvm.part2effects

import cats.effect.IO

import scala.util.{Failure, Success, Try}

object IOErrorHandling {

  // IO: pure, delay, defer
  // create failed effects
  val aFailedCompute: IO[Int] = IO.delay(throw new RuntimeException("A FAILURE"))
  val aFailure: IO[Int] = IO.raiseError(new RuntimeException("a proper fail"))

  // handle exceptions
  val dealWithIt = aFailure.handleErrorWith {
    case _: RuntimeException => IO.delay(println("I'm still here"))
    // add more cases
  }

  // turn it into an IO
  val effectAsEither: IO[Either[Throwable, Int]] = aFailure.attempt

  // redeem: transform the failure and the success in one go
  val resultAsString = aFailure.redeem(ex => s"FAIL: $ex", value => s"SUCCESS: $value")

  val resultAsEffect = aFailure.redeemWith(ex => IO(println(s"FAIL: $ex")), value => IO(println(s"SUCCESS: $value")))

  /**
   * Exercises
   */
  // 1- construct potentially failed IOs from standard data types
  def option2IO[A](option: Option[A])(ifEmpty: Throwable): IO[A] = {
    option match {
      case Some(value) => IO(value)
      case None => IO.raiseError(ifEmpty)
    }
  }

  def try2IO[A](aTry: Try[A]): IO[A] = {
    aTry match {
      case Success(value) => IO(value)
      case Failure(ex) => IO.raiseError(ex)
    }
  }

  def either2IO[A](anEither: Either[Throwable, A]): IO[A] = anEither match {
    case Left(ex) => IO.raiseError(ex)
    case Right(value) => IO(value)
  }
  // IO already has these APIs e.g. fromTry, fromEither, fromFuture, fromOption

  // 2 - handleError, handleErrorWith
  def handleIOError[A](io: IO[A])(handler: Throwable => A): IO[A] = {
//    io.redeem(ex => handler(ex), value => value)
    io.redeem(handler, identity)
  }

  def handleIOErrorWith[A](io: IO[A])(handler: Throwable => IO[A]): IO[A] = {
//    io.redeemWith(ex => handler(ex), value => IO.pure(value))
    io.redeemWith(handler, IO.pure)
  }

  def main(args: Array[String]): Unit = {
    import cats.effect.unsafe.implicits.global

    println(resultAsString.unsafeRunSync())
    println(resultAsEffect.unsafeRunSync())
  }
}
