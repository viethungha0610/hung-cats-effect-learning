package com.rockthejvm.part2effects

import cats.Parallel
import cats.effect.{IO, IOApp}

object IOParallelism extends IOApp.Simple {

  // IOs are usually sequential

  val anisIO = IO(s"[${Thread.currentThread().getName}] Ani")
  val kamranIO = IO(s"[${Thread.currentThread().getName}] Kamran")

  val composedIO = for {
    ani <- anisIO
    kamran <- kamranIO
  } yield s"$ani and $kamran love Rock the JVM"

  // debug extension method
  import com.rockthejvm.utils._

  import cats.syntax.apply._

  val meaningOfLife: IO[Int] = IO.delay(42).debug
  val favLang: IO[String] = IO.delay("Scala").debug
  val goalInLife = (meaningOfLife, favLang).mapN((num, string) => s"my goal in life is $num and $string")

  // parallelism on IOs
  // convert a sequential IO to parallel IO
  val parIO1: IO.Par[Int] = Parallel[IO].parallel(meaningOfLife.debug)
  val parIO2: IO.Par[String] = Parallel[IO].parallel(favLang.debug)
  import cats.effect.implicits._
  val goalInLifeParallel = (parIO1, parIO2).mapN((num, string) => s"my goal in life is $num and $string")
  // turn back to sequential
  val goalInLife_v2 = Parallel[IO].sequential(goalInLifeParallel)

  // shorthand:
  import cats.syntax.parallel._
  val goalInLife_v3: IO[String] = (meaningOfLife, favLang).parMapN((num, string) => s"my goal in life is $num and $string")

  // regarding failure

  val aFailure: IO[String] = IO.raiseError(new RuntimeException("I can't do this!"))
  // compose a success + failure
  val parallelWithFailure = (meaningOfLife.debug, aFailure.debug).parMapN(_ + _)

  // compose a failure + failure
  val anotherFailure: IO[String] = IO.raiseError(new RuntimeException("Second failure"))
  val twoFailures: IO[String] = (aFailure.debug, anotherFailure.debug).parMapN(_ + _)

  // the first effect to fail gives the failure of the result
  val twoFailuresDelayed: IO[String] = (IO(Thread.sleep(1000)) >> aFailure.debug, anotherFailure.debug).parMapN(_ + _)

  override def run: IO[Unit] = {
    twoFailuresDelayed.debug.void
  }
}
