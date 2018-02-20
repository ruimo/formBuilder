package com.ruimo.forms

import javafx.fxml.{FXML, Initializable}
import java.net.URL
import java.util.ResourceBundle
import javafx.scene.control.{ComboBox, TextField}

import scalafx.collections.ObservableBuffer
import scalafx.scene.control.{ComboBox => SfxComboBox}

sealed trait DotRemovalDetailValidation
case object MaxDotSizeInvalid extends DotRemovalDetailValidation
case object BlackLevelInvalid extends DotRemovalDetailValidation

class DotRemovalDetailController extends Initializable {
  @FXML
  private[this] var maxDotSizeText: TextField = _

  @FXML
  private[this] var blackLevelText: TextField = _

  def maxDotSize_=(maxDotSize: Int) {
    maxDotSizeText.setText(maxDotSize.toString)
  }

  def maxDotSize: Int = maxDotSizeText.getText.toInt

  def blackLevel_=(blackLevel: BlackLevel) {
    blackLevelText.setText(blackLevel.value.toString)
  }

  def blackLevel: BlackLevel = BlackLevel(blackLevelText.getText.toInt)

  def validate: Option[DotRemovalDetailValidation] = {
    val v = 
    try {
      val value = maxDotSize
      if (value < 1) Some(MaxDotSizeInvalid) else None
    }
    catch {
      case e: NumberFormatException =>
        Some(MaxDotSizeInvalid)
    }

    v.orElse(
      try {
        blackLevel
        None
      }
      catch {
        case e: NumberFormatException =>
          Some(BlackLevelInvalid)
        case e: IllegalArgumentException =>
          Some(BlackLevelInvalid)
      }
    )
  }

  def model: DotRemovalCondition = DotRemovalConditionImpl(
    maxDotSize = maxDotSize,
    blackLevel = blackLevel
  )

  def model_=(newModel: DotRemovalCondition) {
    maxDotSize = newModel.maxDotSize
    blackLevel = newModel.blackLevel
  }

  override def initialize(url: URL, resourceBundle: ResourceBundle) {
    println("DotRemovalDetailController initialize")
  }
}
