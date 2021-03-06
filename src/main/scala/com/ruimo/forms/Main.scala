package com.ruimo.forms

import java.io._

import scala.concurrent.ExecutionContext.Implicits.global
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
import org.slf4j.LoggerFactory

import scala.concurrent.Future

object Main extends JFXApp with HasLogger {
  val Logger = LoggerFactory.getLogger(getClass)
  val resource = getClass.getResource("main.fxml")
  if (resource == null) {
    throw new RuntimeException("Cannot load resource: main.fxml")
  }

  {
    Thread.setDefaultUncaughtExceptionHandler(new Thread.UncaughtExceptionHandler() {
      def uncaughtException(t: Thread, e: Throwable) {
        logger.error("Uncaught exception.", e)
      }
    })

    logger.info("Application start. Version: " + generated.BuildInfo.version)
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
    logger.info("Terminate application...")
    Ws.shutdown()
    logger.info("WS shutdown requested...")
  }
}
