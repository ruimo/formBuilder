package com.ruimo.forms

import javafx.fxml.{FXML, Initializable}
import java.net.URL
import java.util.ResourceBundle

import javafx.scene.control.{ComboBox, TextField}
import org.slf4j.LoggerFactory
import scalafx.collections.ObservableBuffer
import scalafx.scene.control.{ComboBox => SfxComboBox}

class CropDetailController extends Initializable with HasLogger {
  @FXML
  private[this] var topSensivityText: TextField = _

  @FXML
  private[this] var bottomSensivityText: TextField = _

  @FXML
  private[this] var leftSensivityText: TextField = _

  @FXML
  private[this] var rightSensivityText: TextField = _

  def topSensivity_=(topSensivity: EdgeCropSensivity) {
    topSensivityText.setText(topSensivity.value.toString)
  }

  def topSensivity: EdgeCropSensivity = EdgeCropSensivity(topSensivityText.getText.toInt)

  def bottomSensivity_=(bottomSensivity: EdgeCropSensivity) {
    bottomSensivityText.setText(bottomSensivity.value.toString)
  }

  def bottomSensivity: EdgeCropSensivity = EdgeCropSensivity(bottomSensivityText.getText.toInt)

  def leftSensivity_=(leftSensivity: EdgeCropSensivity) {
    leftSensivityText.setText(leftSensivity.value.toString)
  }

  def leftSensivity: EdgeCropSensivity = EdgeCropSensivity(leftSensivityText.getText.toInt)

  def rightSensivity_=(rightSensivity: EdgeCropSensivity) {
    rightSensivityText.setText(rightSensivity.value.toString)
  }

  def rightSensivity: EdgeCropSensivity = EdgeCropSensivity(rightSensivityText.getText.toInt)

  override def initialize(url: URL, resourceBundle: ResourceBundle) {
    logger.info("CropDetailController initialize")
  }
}
