package com.rockthejvm.part3concurrency

import cats.effect.{IO, IOApp, Resource}

import java.io.{File, FileReader}
import java.util.Scanner
import scala.concurrent.duration.DurationInt

object Resources extends IOApp.Simple {

  import com.rockthejvm.utils._

  class Connection(url: String) {
    def open(): IO[String] = IO(s"opening connection to $url").debug

    def close(): IO[String] = IO(s"closing connection to $url").debug
  }

  val asyncFetchUrl = for {
    fib <- (new Connection("rockthejvm")).open() *> IO.sleep((Int.MaxValue).seconds).start
    _ <- IO.sleep(1.second) *> fib.cancel
  } yield()

  val correctAsyncFetchUrl = for {
    conn <- IO(new Connection("rockthejvm.com"))
    fib <- conn.open() *> IO.sleep((Int.MaxValue).seconds).onCancel(conn.close().void).start
    _ <- IO.sleep(1.second) *> fib.cancel
  } yield ()

  // bracket pattern
  val bracketFetchUrl = IO(new Connection("rockthejvm.com"))
    .bracket(conn => conn.open() *> IO.sleep(Int.MaxValue.seconds))(conn => conn.close().void)

  val bracketProgram = for {
    fib <- bracketFetchUrl.start
    _ <- IO.sleep(1.seconds) *> fib.cancel
  } yield ()

  /*
  * bracket pattern: someIO.bracket(useResourceCb)(releaseResourceCb)
  * bracket is equivalent to try-catches (pure FP)
  * */

  def openFileScanner(path: String): IO[Scanner] = {
    IO(new Scanner(new FileReader(new File(path))))
  }

  def bracketReadFile(path: String): IO[Unit] = {
    IO(s"Opening file at $path") *>
      openFileScanner(path).bracket({
        scanner =>
          def readLineByLine(): IO[Unit] = {
            if (scanner.hasNextLine) {
              IO(scanner.nextLine()).debug >> IO.sleep(100.millis) >> readLineByLine()
            }
            else IO.unit
          }
          readLineByLine()
      })({
        scanner => IO(s"closing file at $path").debug >> IO(scanner.close())
      })
  }

  def connFromConfig(path: String): IO[Unit] = {
    openFileScanner(path)
      .bracket({
        scanner =>
          IO(new Connection(scanner.nextLine())).bracket {
            conn => conn.open() >> IO.never
          }(conn => conn.close().void)
      })({
        scanner => IO("closing file").debug *> IO(scanner.close())
      })
  }

  // nesting resources are tedious

  val connectionResource = Resource.make(IO(new Connection("rockthejvm.com")))(conn => conn.close().void)
  // ... at a later part of your code

  val resourceFetchUrl = for {
    fib <- connectionResource.use(conn => conn.open() >> IO.never).start
    _ <- IO.sleep(1.second) >> fib.cancel
  } yield ()

  // resources are equivalent to brackets
  val simpleResource = IO("some resource")
  val usingResource: String => IO[String] = string => IO(s"using the string: $string").debug
  val releaseResource: String => IO[Unit] = string => IO(s"finalizing the string $string").debug.void

  val usingResourceWithBracket = simpleResource.bracket(usingResource)(releaseResource)
  val usingResourceWithResource = Resource.make(simpleResource)(releaseResource).use(usingResource)


  def readLineByLine(scanner: Scanner): IO[Unit] = {
    if (scanner.hasNextLine) IO(scanner.nextLine()).debug >> IO.sleep(100.millis) >> readLineByLine(scanner)
    else IO.unit
  }

  def getResourceFromFile(path: String) = Resource.make(openFileScanner(path)) { scanner =>
    IO("closing file").debug *> IO(scanner.close())
  }

  def resourceReadFile(path: String) = getResourceFromFile(path).use {
    scanner => readLineByLine(scanner)
  }

  def cancelReadFile(path: String) = for {
    fib <- resourceReadFile(path).start
    _ <- IO.sleep(2.seconds) >> fib.cancel
  } yield()

  // nested resources
  def connFromConfResource(path: String) = {
    Resource.make(IO("opening file").debug >> openFileScanner(path))(scanner => IO("closing file").debug >> IO(scanner.close()))
      .flatMap( scanner =>
        Resource.make(IO(new Connection(scanner.nextLine())))(conn => conn.close().void)
      )
  }
  val openConnection = connFromConfResource("src/main/resources/connection.txt").use(conn => conn.open())
  // connection + file will close automatically
  val canceledConnection = for {
    fib <- openConnection.start
    _ <- IO.sleep(1.second) >> IO("cancelling").debug >> fib.cancel
  } yield ()

  override def run = canceledConnection
}
