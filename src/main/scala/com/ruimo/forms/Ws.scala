package com.ruimo.forms

import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import play.api.libs.ws._
import play.api.libs.ws.ahc._

object Ws {
  private implicit val system = ActorSystem()
  system.registerOnTermination {
    println("Akka terminated.")
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
