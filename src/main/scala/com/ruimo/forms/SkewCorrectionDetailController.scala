package com.ruimo.forms

import javafx.fxml.{FXML, Initializable}
import java.net.URL
import java.util.ResourceBundle
import javafx.scene.control.ComboBox

import scalafx.collections.ObservableBuffer
import scalafx.scene.control.{ComboBox => SfxComboBox}

class SkewCorrectionDetailController extends Initializable {
  val HorizontalLineDetection = "横線検出"
  val VerticalLineDetection = "縦線検出"

  @FXML
  private[this] var hvComboBox: ComboBox[String] = _

  lazy val sfxHvComboBox = new SfxComboBox[String](hvComboBox)

  override def initialize(url: URL, resourceBundle: ResourceBundle) {
    println("SkewCorrectionDetailController initialize")
    sfxHvComboBox += HorizontalLineDetection
    sfxHvComboBox += VerticalLineDetection
    sfxHvComboBox.value = HorizontalLineDetection
  }
}
