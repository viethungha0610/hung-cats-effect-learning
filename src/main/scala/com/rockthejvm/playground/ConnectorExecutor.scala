package com.rockthejvm.playground

/**
  * This is the object that needs to be refactored
  * Feel free to add new functions, classes or whatever else you need.
  */
object ConnectorExecutor {

  // val emailClient = new EmailClient
  // val slackClient = new SlackClient
  // val otherClient = new OtherClient
  // ...

  // TODO - I would have these Connector classes and trait in a different file

  // TODO - We could also create custom exception classes to fit the domain e.g. UnsupportedConnectorException, FailedExecutionException, etc.

  // Client can also potentially have a common interface and a method like `open` and `close` to manage lifecycle, prevent resource leak

  trait BaseConnector {
    // Common interface for connectors to be implemented
    // In this example, the operation returns a string, from experience, operations like this often executes side effects e.g. send a HTTP request, execute DB transaction
    // So we can also use cats-effect's IO monad to represent it, but I'll keep it as string on the right side here for simplicity
    def open(): Unit = {}
    def close(): Unit = {}
    def executeOperation(operationName: String, params: Map[String, String]): Either[Throwable, String]
  }

  // Each of these will be in separate files and/or packages, depending on whether the connectors are implemented by Tray.io or third-party / community
  class EmailConnector extends BaseConnector {

    var emailClient: EmailClient = _
    override def open(): Unit = {
      emailClient = new EmailClient
    }

    override def close(): Unit = {
      // TODO - Close the email client's connection here
    }

    override def executeOperation(
        operationName: String,
        params: Map[String, String]
    ): Either[Throwable, String] = {
      val confEither = Option(ConfigurationRepository.findConnectorConfiguration("Slack"))
        .toRight(new RuntimeException("failed to parse slack config"))
      operationName match {
        case "SendEmail" => sendEmail(params, confEither)
        case _           => Left(new IllegalArgumentException(s"$operationName operation is not supported!"))
      }
    }

    def sendEmail(
        params: Map[String, String],
        confEither: Either[RuntimeException, String]
    ): Either[Throwable, String] = {
      for {
        to <- params.get("to").toRight(new RuntimeException("to could not be found!"))
        cc <- params.get("cc").toRight(new RuntimeException("cc could not be found!"))
        bcc <- params.get("bcc").toRight(new RuntimeException("bcc could not be found!"))
        body <- params.get("body").toRight(new RuntimeException("body could not be found!"))
        conf <- confEither
      } yield emailClient.sendEmail(to, cc, bcc, body, conf)
    }
  }

  class SlackConnector extends BaseConnector {
    var slackClient: SlackClient = _

    override def open(): Unit = {
      slackClient = new SlackClient
    }

    override def close(): Unit = {
      // TODO - close Slack connection if needed
    }

    override def executeOperation(
        operationName: String,
        params: Map[String, String]
    ): Either[Throwable, String] = {
      val confEither = Option(ConfigurationRepository.findConnectorConfiguration("Slack"))
        .toRight(new RuntimeException("failed to parse slack config"))
      operationName match {
        case "SendChannelMessage" => sendChannelMessage(params, confEither)
        case "SendPrivateMessage" => sendPrivateMessage(params, confEither)
        case _                    => Left(new IllegalArgumentException(s"$operationName operation is not supported!"))
      }
    }

    def sendChannelMessage(
        params: Map[String, String],
        confEither: Either[RuntimeException, String]
    ): Either[Throwable, String] = {
      for {
        channel <- params.get("channel").toRight(new RuntimeException("channel could not be found!"))
        message <- params.get("message").toRight(new RuntimeException("message could not be found!"))
        conf <- confEither
      } yield slackClient.sendChannelMessage(channel, message, conf)
    }

    def sendPrivateMessage(
        params: Map[String, String],
        confEither: Either[RuntimeException, String]
    ): Either[Throwable, String] = {
      for {
        recipient <- params.get("recipient").toRight(new RuntimeException("recipient could not be found!"))
        message <- params.get("message").toRight(new RuntimeException("message could not be found"))
        conf <- confEither
      } yield slackClient.sendPrivateMessage(recipient, message, conf)
    }
  }

  object ConnectorFactory {
    // If I could change the `executorConnector` signature, I would skip creating this Factory
    def apply(connector: String): Either[IllegalArgumentException, BaseConnector] =
      connector match {
        case "Email" => Right(new EmailConnector)
        case "Slack" => Right(new SlackConnector)
        // Add more connectors below!
        case _ => Left(new IllegalArgumentException(s"$connector connector is not supported!"))
      }
  }

  /**
    * Don't change the signature of this function
    */
  // If I could change the signature of this function, I would also add a client argument to inject the client
  // So the client can be re-used in multiple executorConnector calls, instead of being created in open() method of Connector
  // But maybe creating a client may be safer, e.g. temporary credentials contained by the client is refreshed each time
  // The client should extend a common trait e.g. BaseClient
  def executeConnector(connector: String, operation: String, params: Map[String, String]): String = {
    val maybeConnectorOutput = for {
      connector <- ConnectorFactory(connector)
      output <- {
        connector.open()
        val outputEither = connector.executeOperation(operation, params)
        connector.close()
        outputEither
      }
    } yield output

    maybeConnectorOutput match {
      case Right(output)   =>
        output
      case Left(exception) =>
        // TODO - add logging here, maybe also error callback
        // TODO - I would also use something like org.apache.commons.lang3.exception.ExceptionUtils to properly log error and stacktrace
        // TODO - I would also use scala-logging's StrictLogging instead of println
        println(s"""
            |Something has gone wrong when executing the Connector
            |Message: ${exception.getMessage}
            |Stacktrace: ExceptionUtils.getStacktrace(exception) <- I would also do this
            |""".stripMargin)
        "ERROR"
    }
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

object Solution {

  /**
    * Do not edit this function
    * It's required by the platform
    */
  def solution(connector: String, operation: String, params: Array[String]): String = {
    ConnectorExecutor.executeConnector(connector, operation, parseParams(params))
  }

  /**
    * Do not edit this function
    * You can assume this parsing is capable of handling correctly all the input
    */
  def parseParams(params: Array[String]): Map[String, String] =
    params.toList.map { param =>
      param.split(":").toList match {
        case key :: value :: Nil => key -> value
        case _                   => throw new Exception("Don't worry, there are not test cases passing through here")
      }
    }.toMap

}

/*
      ______             ___
     / _____)           / __)
    | /      ___  ____ | |__
    | |     / _ \|  _ \|  __)
    | \____| |_| | | | | |
     \______)___/|_| |_|_|

 */

/**
  * Do not edit this class
  * Ignore its implementation and treat it as a black box
  * (check its signatures and docs)
  */
object ConfigurationRepository {

  /**
    * @return the configuration for the specified connector or null if the configuration is missing
    */
  def findConnectorConfiguration(name: String): String = {
    if (name == "Slack") {
      "SLACK CONF"
    } else {
      null
    }
  }

}

/*

          ______ _ _
         / _____) (_)            _
        | /     | |_  ____ ____ | |_   ___
        | |     | | |/ _  )  _ \|  _) /___)
        | \_____| | ( (/ /| | | | |__|___ |
         \______)_|_|\____)_| |_|\___|___/


 */

/**
  * Do not edit this class
  * Ignore its implementation and treat it as a black box
  * (check its signatures and docs)
  */
class EmailClient {

  /**
    * @throws NullPointerException if any parameter is null
    */
  def sendEmail(to: String, cc: String, bcc: String, body: String, configuration: String) = "EMAIL OUTPUT"
}

/**
  * Do not edit this class
  * Ignore its implementation and treat it as a black box
  * (check its signatures and docs)
  */
class SlackClient {

  /**
    * @throws NullPointerException if any parameter is null
    */
  def sendChannelMessage(channel: String, message: String, configuration: String) =
    "SLACK CHANNEL MESSAGE OUTPUT"

  /**
    * @throws NullPointerException if any parameter is null
    */
  def sendPrivateMessage(recipient: String, message: String, configuration: String) =
    "SLACK PRIVATE MESSAGE OUTPUT"
}
