package com.rockthejvm.playground

import scala.util.{Failure, Success, Try}

object Solution {

  /**
   * This is the function that needs to be implemented
   * Don't change its signature
   */
  def getTotalScore(usersIds: Seq[Int]): String = {
    /**
     * Again, similar to last exercise, instead of println, I would use a logging library, I normally use the StrictLogging trait of the
     * scala-logging library and ExceptionUtils class (org.apache.commons.lang3.exception) to get error messages and stacktrace
     */

    import ScoreRepository._
    val totalScore = for {
      users <- UserRepository.getUsers(usersIds).toEither
      scores = users.map(user => getUserScore(user) match {
        case Success(score) => score
        case Failure(throwable) =>
          // I would confirm with Product Managers or something with the domain knowledge
          // if returning 0 here is the right behaviour when unable to get individual score
          println(
            s"""
              |Something has gone wrong when calculating the individual score for user with id ${user.id}, assigning score to 0 for now
              |Message: ${throwable.getMessage}
              |Stacktrace: ExceptionUtils.getStacktrace(exception) <- I would also do this
              |""".stripMargin)
          0
      })
    } yield scores.sum

    totalScore match {
      case Right(totalScore) => totalScore.toString
      case Left(throwable) =>
        println(
          s"""
            |Something has gone wrong when getting the total score
            |Message: ${throwable.getMessage}
            |Stacktrace: ExceptionUtils.getStacktrace(exception) <- I would also do this
            |""".stripMargin)
        "ERROR"
    }
  }


  /*
       _____       _
      (_____)     (_)_
         _   ____  _| |_
        | | |  _ \| |  _)
       _| |_| | | | | |__
      (_____)_| |_|_|\___)

   */


  /**
   * Do not edit this function
   * It's required by the platform
   * You can assume it is capable of handling correctly all the input
   */
  def solution(input: Array[Int]): String = {
    getTotalScore(input.map(_.toInt))
  }

}

/*
        _
       | |
        \ \   ____ ___   ____ ____  ___
         \ \ / ___) _ \ / ___) _  )/___)
     _____) | (__| |_| | |  ( (/ /|___ |
    (______/ \____)___/|_|   \____|___/

 */

/**
 * Do not edit this class
 * Ignore its implementation and treat it as a black box
 * (check its signatures and docs)
 */
object ScoreRepository {

  val scores = Map(
    User(1) -> Success(23),
    User(2) -> Failure(new Exception("Something went wrong")),
    User(3) -> Success(19)
  )

  def getUserScore(user: User): Try[Int] = {
    Try(scores(user)).flatten
  }

}

/*
     _     _
    | |   | |
    | |   | | ___  ____  ____ ___
    | |   | |/___)/ _  )/ ___)___)
    | |___| |___ ( (/ /| |  |___ |
     \______(___/ \____)_|  (___/

 */

/**
 * Do not edit this class
 * Ignore its implementation and treat it as a black box
 * (check its signatures and docs)
 */
final case class User(id: Int)

/**
 * Do not edit this class
 * Ignore its implementation and treat it as a black box
 * (check its signatures and docs)
 */
object UserRepository {

  val users = Map(
    1 ->  User(1),
    2 ->  User(2),
    3 ->  User(3)
  )

  def getUsers(ids: Seq[Int]): Try[Seq[User]] = {
    Try(ids.map(users(_)))
  }

}
