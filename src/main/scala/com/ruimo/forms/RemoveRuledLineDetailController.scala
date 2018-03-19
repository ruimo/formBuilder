package com.ruimo.forms

import javafx.fxml.{FXML, Initializable}
import java.net.URL
import java.util.ResourceBundle
import javafx.scene.control.{ComboBox, TextField, CheckBox}
import javafx.event.ActionEvent

import scalafx.collections.ObservableBuffer
import scalafx.scene.control.{ComboBox => SfxComboBox}

sealed trait RemoveRuledLineDetailValidation
case object InvalidLineDeltaX extends RemoveRuledLineDetailValidation
case object InvalidLineDeltaY extends RemoveRuledLineDetailValidation
case object InvalidLineDotRatio extends RemoveRuledLineDetailValidation
case object InvalidCorrectOverlappingDelta extends RemoveRuledLineDetailValidation
case object InvalidCorrectOverlappingDotRatio extends RemoveRuledLineDetailValidation

class RemoveRuledLineDetailController extends Initializable {

  @FXML
  private[this] var lineDeltaXText: TextField = _

  def lineDeltaX_=(ea: Int) {
    lineDeltaXText.setText(ea.toString)
  }

  def lineDeltaX: Int = lineDeltaXText.getText.toInt

  @FXML
  private[this] var lineDeltaYText: TextField = _

  def lineDeltaY_=(ea: Int) {
    lineDeltaYText.setText(ea.toString)
  }

  def lineDeltaY: Int = lineDeltaYText.getText.toInt

  @FXML
  private[this] var lineDotRatioText: TextField = _

  def lineDotRatio_=(ea: Int) {
    lineDotRatioText.setText(ea.toString)
  }

  def lineDotRatio: Int = lineDotRatioText.getText.toInt

  @FXML
  private[this] var correctOverlappingEnabledCheckBox: CheckBox = _

  def correctOverlappingEnabled_=(ea: Boolean) {
    correctOverlappingEnabledCheckBox.setSelected(ea)
  }

  def correctOverlappingEnabled: Boolean = correctOverlappingEnabledCheckBox.isSelected

  @FXML
  def correctOverlappingEnabledClicked(e: ActionEvent) {
    println("correctOverlappingEnabledClicked")
    correctOverlappingDeltaText.setDisable(!correctOverlappingEnabled)
    correctOverlappingDotRatioText.setDisable(!correctOverlappingEnabled)
  }

  @FXML
  private[this] var correctOverlappingDeltaText: TextField = _

  def correctOverlappingDelta_=(ea: Int) {
    correctOverlappingDeltaText.setText(ea.toString)
  }

  def correctOverlappingDelta: Int = correctOverlappingDeltaText.getText.toInt


  @FXML
  private[this] var correctOverlappingDotRatioText: TextField = _

  def correctOverlappingDotRatio_=(ea: Int) {
    correctOverlappingDotRatioText.setText(ea.toString)
  }

  def correctOverlappingDotRatio: Int = correctOverlappingDotRatioText.getText.toInt


  def validate: Option[RemoveRuledLineDetailValidation] = {
    (try {
      val value = lineDeltaX
      if (value <= 0) Some(InvalidLineDeltaX) else None
    }
    catch {
      case e: NumberFormatException =>
        Some(InvalidLineDeltaX)
    }).orElse {
      try {
        val value = lineDeltaY
        if (value <= 0) Some(InvalidLineDeltaY) else None
      }
      catch {
        case e: NumberFormatException =>
          Some(InvalidLineDeltaY)
      }
    }.orElse {
      try {
        val value = lineDotRatio
        if (value <= 0 || value > 100) Some(InvalidLineDotRatio) else None
      }
      catch {
        case e: NumberFormatException =>
          Some(InvalidLineDotRatio)
      }
    }.orElse {
      try {
        val value = correctOverlappingDelta
        if (value <= 0) Some(InvalidCorrectOverlappingDelta) else None
      }
      catch {
        case e: NumberFormatException =>
          Some(InvalidCorrectOverlappingDelta)
      }
    }.orElse {
      try {
        val value = correctOverlappingDotRatio
        if (value <= 0 || value > 100) Some(InvalidCorrectOverlappingDotRatio) else None
      }
      catch {
        case e: NumberFormatException =>
          Some(InvalidCorrectOverlappingDotRatio)
      }
    }
  }


  def model: RemoveRuledLineCondition = RemoveRuledLineConditionImpl(
    lineDeltaX = lineDeltaX,
    lineDeltaY = lineDeltaY,
    lineDotRatio = lineDotRatio,
    correctOverlappingEnabled = correctOverlappingEnabled,
    correctOverlappingDelta = correctOverlappingDelta,
    correctOverlappingDotRatio = correctOverlappingDotRatio
  )

  def model_=(newModel: RemoveRuledLineCondition) {
    lineDeltaX = newModel.lineDeltaX
    lineDeltaY = newModel.lineDeltaY
    lineDotRatio = newModel.lineDotRatio
    correctOverlappingEnabled = newModel.correctOverlappingEnabled
    correctOverlappingDelta = newModel.correctOverlappingDelta
    correctOverlappingDotRatio = newModel.correctOverlappingDotRatio
  }

  override def initialize(url: URL, resourceBundle: ResourceBundle) {
    println("RemoveRuledLineDetailController initialize")
  }
}
