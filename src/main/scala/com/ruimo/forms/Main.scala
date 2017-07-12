package com.ruimo.forms

import javafx.{fxml => jfxf}
import javafx.{scene => jfxs}
import scalafx.Includes._
import scalafx.application.JFXApp
import scalafx.application.JFXApp.PrimaryStage
import scalafx.scene.Scene

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

    val controller: MainController = loader.getController().asInstanceOf[MainController]
    controller.setStage(stage);
  }
}
