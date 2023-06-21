package com.rockthejvm.part3concurrency

import cats.effect.kernel.Outcome.{Canceled, Errored, Succeeded}
import cats.effect.{Fiber, IO, IOApp, Outcome}

import scala.concurrent.duration.{DurationInt, FiniteDuration}

object Fibers extends IOApp.Simple {

  import com.rockthejvm.utils._

  val meaningOfLife = IO.pure(42)
  val favLang = IO.pure("Scala")

  def simpleIOComposition() = for {
    mol <- meaningOfLife.debug
    lang <- favLang.debug
  } yield ()

  // introduce the Fiber
  def createFiber: Fiber[IO, Throwable, String] = ??? // almost impossible to create fibers manually

  // the fiber is nto actually started, but the fiber allocation is wrapped in another effect
  val aFiber: IO[Fiber[IO, Throwable, Int]] = meaningOfLife.debug.start

  def differentThreadIOs() = for {
    _ <- aFiber
    _ <- favLang.debug
  } yield ()

  // joining a fiber
  def runOnSomeOtherThread[A](io: IO[A]): IO[Outcome[IO, Throwable, A]] = for {
    fib <- io.start
    result <- fib.join // an effect which waits for the fiber to terminate
  } yield result
  /*
  * IO[ResultType of fib.join]
  * fib.join = Outcome[IO[A]]
  * possible outcomes:
  * - success with an IO
  * - failure with an exception
  * - cancelled
  * */

  val someIOOnAnotherThread = runOnSomeOtherThread(meaningOfLife) // IO(Succeeded(IO(42)))

  val someResultFromAnotherThread = someIOOnAnotherThread.flatMap {
    case Succeeded(effect) => effect
    case Errored(e) => IO(0)
    case Canceled() => IO(0)
  }

  def throwOnAnotherThread() = for {
    fib <- IO.raiseError[Int](new RuntimeException("no number for you")).start
    result <- fib.join
  } yield result

  def testCancel() = {
    val task = IO("starting").debug >> IO.sleep(1.seconds) >> IO("done").debug
    val taskWithCancellationHandler = task.onCancel(IO("I'm being cancelling!").debug.void)

    for {
      fib <- taskWithCancellationHandler.start // on a separate thread
      _ <- IO.sleep(500.millis) >> IO("cancelling")
      _ <- fib.cancel
      result <- fib.join
    } yield result
  }

  /**
   * Exercises
   */

  def processResultsFromFiber[A](io: IO[A]): IO[A] = {
    val ioResult = for {
      fib <- io.debug.start
      result <- fib.join
    } yield result

    ioResult.flatMap {
      case Succeeded(fa) => fa
      case Errored(e) => IO.raiseError(e)
      case Canceled() => IO.raiseError(new RuntimeException("Computation canceled."))
    }
  }

  def testEx1() = {
    val aComputation = IO("starting").debug >> IO.sleep(1.seconds) >> IO("done!").debug >> IO(42)
    processResultsFromFiber(aComputation).void
  }

  def tupleIO[A, B](ioa: IO[A], iob: IO[B]): IO[(A, B)] = {
    val result = for {
      fiba <- ioa.start
      fibb <- iob.start
      resulta <- fiba.join
      resultb <- fibb.join
    } yield (resulta, resultb)

    result.flatMap {
      case (Succeeded(fa), Succeeded(fb)) => for {
        a <- fa
        b <- fb
      } yield (a, b)
      case (Errored(e), _) => IO.raiseError(e)
      case (_, Errored(e)) => IO.raiseError(e)
      case _ => IO.raiseError(new RuntimeException("Some computation canceled."))
    }
  }

  def testEx2() = {
    val firstIO = IO.sleep(2.seconds) >> IO(1).debug
    val secondIO = IO.sleep(3.seconds) >> IO(2).debug

    tupleIO(firstIO, secondIO).debug.void
  }

  def timeout[A](io: IO[A], duration: FiniteDuration): IO[A] = {
    val computation = for {
      fib <- io.start
      _ <- IO.sleep(duration) >> fib.cancel
      result <- fib.join
    } yield result

    computation.flatMap {
      case Succeeded(fa) => fa
      case Errored(e) => IO.raiseError(e)
      case Canceled() => IO.raiseError(new RuntimeException("Computation canceled."))
    }
  }

  def testEx3() = {
    val aComputation = IO("starting").debug >> IO.sleep(1.seconds) >> IO("done!").debug >> IO(42)
    timeout(aComputation, 500.millis).debug.void
  }

  override def run: IO[Unit] = {
    testEx3()
  }
}
