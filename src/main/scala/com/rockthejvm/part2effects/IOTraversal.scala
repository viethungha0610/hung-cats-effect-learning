package com.rockthejvm.part2effects

import cats.Traverse
import cats.effect.{IO, IOApp}

import scala.concurrent.Future
import scala.util.Random

object IOTraversal extends IOApp.Simple {

  import scala.concurrent.ExecutionContext.Implicits.global

  def heavyComputation(string: String): Future[Int] = Future {
    Thread.sleep(Random.nextInt(1000))
    string.split(" ").length
  }

  val workLoad: List[String] = List("I quite like CE", "Scala is great", "looking forward to some awesome stuff")

  def clunkyFutures(): Unit = {
    val futures: List[Future[Int]] = workLoad.map(heavyComputation)
    // Future[List[Int]]
    futures.foreach(_.foreach(println))
  }


  // traverse

  import cats.instances.list._

  val listTraverse = Traverse[List]

  def traverseFutures() = {

    val singleFuture: Future[List[Int]] = listTraverse.traverse(workLoad)(heavyComputation)
    // ^^ this stores ALL the result

    singleFuture.foreach(println)
  }

  import com.rockthejvm.utils._
  def computeAsIO(string: String): IO[Int] = IO {
    Thread.sleep(Random.nextInt(1000))
    string.split(" ").length
  }.debug

  val ios: List[IO[Int]] = workLoad.map(computeAsIO)
  val singleIO: IO[List[Int]] = listTraverse.traverse(workLoad)(computeAsIO)

  // parallel traversal
  import cats.syntax.parallel._
  val parallelSingleIO: IO[List[Int]] = workLoad.parTraverse(computeAsIO)


  /**
   * Exercises
   */

  def sequence[A](listOfIOs: List[IO[A]]): IO[List[A]] =
    listTraverse.traverse(listOfIOs)(x => x)

  def sequence_v2[F[_]: Traverse, A](listOfIOs: F[IO[A]]): IO[F[A]] =
    Traverse[F].traverse(listOfIOs)(x => x)

  def parSequence[A](listOfIOs: List[IO[A]]): IO[List[A]] =
    listOfIOs.parTraverse(x => x)

  def parSequence_v2[F[_]: Traverse, A](wrapperOfIOs: F[IO[A]]): IO[F[A]] =
    wrapperOfIOs.parTraverse(x => x)

  // sequence turns a double nested container inside-out
  val singleIO_v2: IO[List[Int]] = listTraverse.sequence(ios)


  override def run: IO[Unit] =
    parallelSingleIO.map(_.sum).debug.void
}

