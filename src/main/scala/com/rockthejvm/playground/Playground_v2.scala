package com.rockthejvm.playground

object Playground_v2 {


  import scala.collection.JavaConverters._

  // you can write to stdout for debugging purposes, e.g.
  // println("this is a debug message")

  /**
   * This is the object that needs to be refactored
   * Feel free to add new functions, classes or whatever else you need.
   */
  object ConnectorExecutor {
    val emailClient = new EmailClient
    val slackClient = new SlackClient
    // val otherClient = new OtherClient
    // ...

    /**
     * Don't change the signature of this function
     */
    def executeConnector(connector: String, operation: String, params: Map[String, String]): String = {
      if (connector == "Slack") {
        if (operation == "SendChannelMessage") {
          val channel = params("channel")
          val message = params("message")
          val conf = ConfigurationRepository.findConnectorConfiguration("Slack")
          slackClient.sendChannelMessage(channel, message, conf)
        } else if (operation == "SendPrivateMessage") {
          val recipient = params("recipient")
          val message = params("message")
          val conf = ConfigurationRepository.findConnectorConfiguration("Slack")
          slackClient.sendPrivateMessage(recipient, message, conf)
        } else {
          "ERROR"
        }
      } else if (connector == "Email") {
        if (operation == "SendEmail") {
          val to = params("to")
          val cc = params("cc")
          val bcc = params("bcc")
          val body = params("body")
          val conf = ConfigurationRepository.findConnectorConfiguration("Email")
          emailClient.sendEmail(to, cc, bcc, body, conf)
        } else {
          "ERROR"
        }

      } else {
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
    def parseParams(params: Array[String]): Map[String, String] = params.toList.map { param =>
      param.split(":").toList match {
        case key :: value :: Nil => key -> value
        case _ => throw new Exception("Don't worry, there are not test cases passing through here")
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
    def sendEmail(to: String , cc:String , bcc: String , body: String , configuration: String) = "EMAIL OUTPUT"
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
    def sendChannelMessage(channel: String, message: String, configuration: String) = "SLACK CHANNEL MESSAGE OUTPUT"

    /**
     * @throws NullPointerException if any parameter is null
     */
    def sendPrivateMessage (recipient: String, message: String, configuration: String) = "SLACK PRIVATE MESSAGE OUTPUT"
  }


  def main(args: Array[String]): Unit = {
    println("Hello World!")
  }

}
