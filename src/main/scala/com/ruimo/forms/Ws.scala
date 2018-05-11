package com.ruimo.forms

import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import org.slf4j.LoggerFactory
import play.api.libs.ws._
import play.api.libs.ws.ahc._

object Ws extends HasLogger {
  private implicit val system = ActorSystem()
  system.registerOnTermination {
    logger.info("Akka terminated.")
    System.exit(0)
  }

  def shutdown() {
    system.terminate()
  }

  lazy val instance: StandaloneWSClient = {
    implicit val materializer = ActorMaterializer()
    StandaloneAhcWSClient()
  }

  def apply() = instance
}
