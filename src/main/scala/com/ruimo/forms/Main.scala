package com.ruimo.forms

import javafx.event.EventHandler
import javafx.stage.WindowEvent
import javafx.{fxml => jfxf}
import javafx.{scene => jfxs}

import scalafx.Includes._
import scalafx.application.JFXApp
import scalafx.application.JFXApp.PrimaryStage
import scalafx.scene.Scene
import akka.actor.ActorSystem

import scalafx.application.Platform
import akka.stream.ActorMaterializer

object Main extends JFXApp {
  val resource = getClass.getResource("main.fxml")
  if (resource == null) {
    throw new RuntimeException("Cannot load resource: main.fxml")
  }

  {
    val loader = new jfxf.FXMLLoader(resource)
    val root: jfxs.Parent = loader.load()

    stage = new PrimaryStage() {
      title = "Form Builder"
      scene = new Scene(root)
    }

    stage.onCloseRequest = new EventHandler[WindowEvent] {
      override def handle(t: WindowEvent): Unit = {
        terminate()
      }
    }

    val controller: MainController = loader.getController().asInstanceOf[MainController]
    controller.setStage(stage)
  }

  def terminate() {
    println("Terminate application...")
    Ws.shutdown()
    println("WS shutdown requested...")
  }
}
