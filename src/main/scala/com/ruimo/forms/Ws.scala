package com.ruimo.forms

import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import play.api.libs.ws._
import play.api.libs.ws.ahc._
import DefaultBodyReadables._
import scala.concurrent.ExecutionContext.Implicits._

object Ws {
  private implicit val system = ActorSystem()

  def shutdown() {
    system.terminate()
  }

  lazy val instance: StandaloneWSClient = {
    system.registerOnTermination {
      System.exit(0)
    }
    implicit val materializer = ActorMaterializer()
    StandaloneAhcWSClient()
  }

  def apply() = instance
}
