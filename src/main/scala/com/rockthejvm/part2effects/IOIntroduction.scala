package com.rockthejvm.part2effects

import cats.effect.IO

import scala.io.StdIn

object IOIntroduction {

  // IO
  val ourFirstIO: IO[Int] = IO.pure(42) // arg that should not have side effects
  val aDelayedIO: IO[Int] = IO.delay({
    println("I'm producing an integer")
    54
  })

  val aDelayedIO_v2: IO[Int] = IO {
    println("I'm producing an integer")
    54
  }

  // map, flatMap
  val improvedMeaningofLife: IO[Int] = ourFirstIO.map(_ * 2)
  val printedMeaningOfLife = ourFirstIO.flatMap(mol => IO.delay(println(mol)))

  def smallProgram(): IO[Unit] = for {
    line1 <- IO(StdIn.readLine())
    line2 <- IO(StdIn.readLine())
    _ <- IO.delay(println(line1 + line2))
  } yield ()

  // mapN - combine IO effects as tuples
  import cats.syntax.apply._
  val combinedMeaningOfLife: IO[Int] = (ourFirstIO, improvedMeaningofLife).mapN(_ + _)

  def smallProgram_v2(): IO[Unit] = {
    (IO(StdIn.readLine()), IO(StdIn.readLine())).mapN(_ + _).map(println)
  }

  // 1- sequence 2 IOs and take the result of the LAST one
  // use flatMap
  def sequenceTakeLast[A, B](ioa: IO[A], iob: IO[B]): IO[B] = {
    ioa.flatMap(_ => iob)
  }

  def sequenceTakeLast_v2[A, B](ioa: IO[A], iob: IO[B]): IO[B] = {
    ioa *> iob // andThen
  }

  def sequenceTakeLast_v3[A, B](ioa: IO[A], iob: IO[B]): IO[B] = {
    ioa >> iob
  }

  // 2 - sequence two IOs and take the result of the FIRST one
  def sequenceTakeFirst[A, B](ioa: IO[A], iob: IO[B]): IO[A] = {
    ioa.flatMap(a => iob.map(_ => a))
  }

  def sequenceTakeFirst_v2[A, B](ioa: IO[A], iob: IO[B]): IO[A] = {
    ioa <* iob
  }

  // 3 - repeat an IO effect forever
  // hint use flatMap + recursion
  def forever[A](io: IO[A]): IO[A] =
    io.flatMap(_ => forever(io))

  def forever_v2[A](io: IO[A]): IO[A] = {
    io >> forever_v2(io)
  }

  def forever_v3[A](io: IO[A]): IO[A] = {
    io *> forever_v3(io)
  }

  def forever_v4[A](io: IO[A]): IO[A] = {
    io.foreverM // with tail recursion
  }

  // 4 - convert an IO to a different type
  def convert[A, B](ioa: IO[A], value: B): IO[B] = {
    ioa.map(_ => value)
  }

  def convert_v2[A, B](ioa: IO[A], value: B): IO[B] = {
    ioa.as(value)
  }

  // 5 - discard value inside an IO, just return Unit
  def asUnit[A](ioa: IO[A]): IO[Unit] = {
    ioa.as(())
  }

  def asUnit_v2[A](ioa: IO[A]): IO[Unit] = {
    ioa.map(_ => ())
  }

  def asUnit_v3[A](ioa: IO[A]): IO[Unit] = {
    ioa.void
  }

  // 6 - fix stack recursion
  def sum(n: Int): Int = {
    if (n <= 0) {
      0
    }
    else {
      n + sum(n - 1)
    }
  }

  def sumIO(n: Int): IO[Int] = {
    if (n <= 0) IO(0)
    else {
      for {
        lastNumber <- IO(n)
        prevSum <- sumIO(n - 1)
      } yield prevSum + lastNumber
    }
  }

  // 7 (hard) - write a fibonacci IO that does NOT crash on recursion
  // hints: use recursion, ignore exponential complexity, use flatMap heavily
  def fibonacci(n: Int): IO[BigInt] = {
    if (n < 2) IO(1)
    else for {
      last <- IO(fibonacci(n - 1)).flatMap(x => x)
      prev <- IO(fibonacci(n - 2)).flatMap(x => x)
    } yield last + prev
  }

  def playground(): Unit = {
    def divideBy(a: Int, b: Int): Option[Int] = {
      if (b != 0) Some(a / b) else None
    }

    val maybeNumber: Option[Int] = Some(10)
    val result: Option[Int] = maybeNumber.flatMap(x => divideBy(x, 2))

    val ioNumber = IO(10)
    val output = ioNumber
      .map(i => IO(i / 2))
      .flatten
  }

  def main(args: Array[String]): Unit = {
    import cats.effect.unsafe.implicits.global // "platform"
    // "end of the world"

    val maybeNumber: Option[Int] = Some(10)

    val flatMappedOutput = maybeNumber
      .flatMap(i => Option(i / 2))
      .flatMap(j => Option(j + 3))

    val flatMappedOutput_v2 = for {
      i <- maybeNumber
      half <- Option(i / 2)
      result <- Option(half + 3)
    } yield result

    println(flatMappedOutput)
    println(flatMappedOutput_v2)
  }
}
