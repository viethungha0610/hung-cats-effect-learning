package com.rockthejvm.part2effects

object Effects {

  // pure functional programming
  // substitution
  def combine(a: Int, b: Int): Int = a + b
  val five = combine(2, 3)
  val five_v2 = 2 + 3
  val five_v3 = 5

  // example: print to the console
  val printSomething: Unit = println("Cats Effect")
  val printSomething_v2: Unit = ()

  // example: change a variable
  var anInt = 0
  val changingVar: Unit = (anInt + 1)
  val changingVar_v2: Unit = () // not the same thing

  // side effects are inevitable for useful programs


  case class MyIO[A](unsafeRun: () => A) {
    def map[B](f: A => B): MyIO[B] =
      MyIO(() => f(unsafeRun()))

    def flatMap[B](f: A => MyIO[B]): MyIO[B] =
      MyIO(() => f(unsafeRun()).unsafeRun())
  }

  val anIO: MyIO[Int] = MyIO(() => {
    println("I'm writing something")
    42
  })

  // referential transparency = can replace an expression with its value without changing behaviour
  def main(args: Array[String]): Unit = {

  }
}
