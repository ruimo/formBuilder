package com.ruimo.forms

import javafx.fxml.{FXML, Initializable}
import java.net.URL
import java.util.ResourceBundle

import com.ruimo.forms.common.{BlackEdge, ColorEdge, Edge, EdgeCropSensitivity}
import javafx.beans.value.{ChangeListener, ObservableValue}
import javafx.scene.control.{ComboBox, TextField}
import org.slf4j.LoggerFactory
import scalafx.collections.ObservableBuffer
import scalafx.scene.control.{ComboBox => SfxComboBox}
import javafx.scene.control.ComboBox
import javafx.scene.layout.GridPane
import scalafx.scene.control.{ComboBox => SfxComboBox, TextField => SfxTextField}
import scalafx.scene.layout.{GridPane => SfxGridPane}

sealed trait CropDetailValidation
object CropDetailValidation {
  case object HvalueInvalid extends CropDetailValidation
  case object SvalueInvalid extends CropDetailValidation
  case object VvalueInvalid extends CropDetailValidation
  case object HsvErrorInvalid extends CropDetailValidation
  case object HsvThresholdInvalid extends CropDetailValidation
  case object TopSensitivityInvalid extends CropDetailValidation
  case object BottomSensitivityInvalid extends CropDetailValidation
  case object LeftSensitivityInvalid extends CropDetailValidation
  case object RightSensitivityInvalid extends CropDetailValidation
}

class CropDetailController extends Initializable with HasLogger {
  sealed trait CropTypeCode
  case object BlackCropCode extends CropTypeCode {
    override def toString = "黒検出"
  }
  case object ColorCropCode extends CropTypeCode {
    override def toString = "色検出"
  }

  @FXML
  private[this] var cropType: ComboBox[CropTypeCode] = _
  lazy val sfxCropTypeComboBox = new SfxComboBox[CropTypeCode](cropType)

  @FXML
  private[this] var topSensivityText: TextField = _
  lazy val sfxTopSensivityText: SfxTextField = new SfxTextField(topSensivityText)

  @FXML
  private[this] var bottomSensivityText: TextField = _
  lazy val sfxBottomSensivityText: SfxTextField = new SfxTextField(bottomSensivityText)

  @FXML
  private[this] var leftSensivityText: TextField = _
  lazy val sfxLeftSensivityText: SfxTextField = new SfxTextField(leftSensivityText)

  @FXML
  private[this] var rightSensivityText: TextField = _
  lazy val sfxRightSensivityText: SfxTextField = new SfxTextField(rightSensivityText)

  @FXML
  private[this] var cropTypeBlackGrid: GridPane = _
  lazy val sfxCropTypeBlackGrid = new SfxGridPane(cropTypeBlackGrid)

  @FXML
  private[this] var cropTypeColorGrid: GridPane = _
  lazy val sfxCropTypeColorGrid = new SfxGridPane(cropTypeColorGrid)

  @FXML
  private[this] var hvalue: TextField = _
  lazy val sfxHvalue = new SfxTextField(hvalue)

  @FXML
  private[this] var svalue: TextField = _
  lazy val sfxSvalue = new SfxTextField(svalue)

  @FXML
  private[this] var vvalue: TextField = _
  lazy val sfxVvalue = new SfxTextField(vvalue)

  @FXML
  private[this] var hsvError: TextField = _
  lazy val sfxHsvError = new SfxTextField(hsvError)

  @FXML
  private[this] var hsvThreshold: TextField = _
  lazy val sfxHsvThreshold = new SfxTextField(hsvThreshold)

  def topSensivity_=(topSensivity: EdgeCropSensitivity) {
    topSensivityText.setText(topSensivity.value.toString)
  }

  def topSensivity: EdgeCropSensitivity = EdgeCropSensitivity(topSensivityText.getText.toInt)

  def bottomSensivity_=(bottomSensivity: EdgeCropSensitivity) {
    bottomSensivityText.setText(bottomSensivity.value.toString)
  }

  def bottomSensivity: EdgeCropSensitivity = EdgeCropSensitivity(bottomSensivityText.getText.toInt)

  def leftSensivity_=(leftSensivity: EdgeCropSensitivity) {
    leftSensivityText.setText(leftSensivity.value.toString)
  }

  def leftSensivity: EdgeCropSensitivity = EdgeCropSensitivity(leftSensivityText.getText.toInt)

  def rightSensivity_=(rightSensivity: EdgeCropSensitivity) {
    rightSensivityText.setText(rightSensivity.value.toString)
  }

  def rightSensivity: EdgeCropSensitivity = EdgeCropSensitivity(rightSensivityText.getText.toInt)

  def gridPane(cropTypeCode: CropTypeCode): SfxGridPane = {
    cropTypeCode match {
      case BlackCropCode => sfxCropTypeBlackGrid
      case ColorCropCode => sfxCropTypeColorGrid
    }
  }

  override def initialize(url: URL, resourceBundle: ResourceBundle) {
    logger.info("CropDetailController initialize")
    sfxCropTypeComboBox += BlackCropCode
    sfxCropTypeComboBox += ColorCropCode
    sfxCropTypeComboBox.value = BlackCropCode
    gridPane(BlackCropCode).visible = true

    sfxCropTypeComboBox.valueProperty.addListener(new ChangeListener[CropTypeCode] {
      override def changed(observableValue: ObservableValue[_ <: CropTypeCode], from: CropTypeCode, to: CropTypeCode) {
        gridPane(from).visible = false
        gridPane(to).visible = true
      }
    })
  }

  def edge_=(newEdge: Edge) {
    newEdge match {
      case blackEdge: BlackEdge =>
        sfxCropTypeComboBox.value = BlackCropCode
        sfxTopSensivityText.text = blackEdge.topSensitivity.value.toString
        sfxBottomSensivityText.text = blackEdge.bottomSensitivity.value.toString
        sfxLeftSensivityText.text = blackEdge.leftSensitivity.value.toString
        sfxRightSensivityText.text = blackEdge.rightSensitivity.value.toString
      case colorEdge: ColorEdge =>
        sfxCropTypeComboBox.value = ColorCropCode
        sfxHvalue.text = colorEdge.h.toString
        sfxSvalue.text = colorEdge.s.toString
        sfxVvalue.text = colorEdge.v.toString
        sfxHsvError.text = colorEdge.errorPercentage.value.toString
        sfxHsvThreshold.text = colorEdge.threshold.value.toString
    }
  }

  def edge: Edge = sfxCropTypeComboBox.value() match {
    case BlackCropCode =>
      BlackEdge(
        EdgeCropSensitivity(sfxTopSensivityText.text.value.toInt),
        EdgeCropSensitivity(sfxBottomSensivityText.text.value.toInt),
        EdgeCropSensitivity(sfxLeftSensivityText.text.value.toInt),
        EdgeCropSensitivity(sfxRightSensivityText.text.value.toInt)
      )
    case ColorCropCode =>
      ColorEdge(hValue, sValue, vValue, hsvErrorValue, hsvThresholdValue)
  }

  def hValue: Double = sfxHvalue.text.value.toDouble
  def sValue: Double = sfxSvalue.text.value.toDouble
  def vValue: Double = sfxVvalue.text.value.toDouble
  def hsvErrorValue: Double = sfxHsvError.text.value.toDouble
  def hsvThresholdValue: Double = sfxHsvThreshold.text.value.toDouble

  def validate: Option[CropDetailValidation] = sfxCropTypeComboBox.value() match {
    case BlackCropCode =>
      (
        try {
          leftSensivity
          None
        } catch {
          case e: IllegalArgumentException => Some(CropDetailValidation.LeftSensitivityInvalid)
          case e: NumberFormatException => Some(CropDetailValidation.LeftSensitivityInvalid)
        }
      ).orElse(
        try {
          rightSensivityText
          None
        } catch {
          case e: IllegalArgumentException => Some(CropDetailValidation.RightSensitivityInvalid)
          case e: NumberFormatException => Some(CropDetailValidation.RightSensitivityInvalid)
        }
      ).orElse(
        try {
          topSensivityText
          None
        } catch {
          case e: IllegalArgumentException => Some(CropDetailValidation.TopSensitivityInvalid)
          case e: NumberFormatException => Some(CropDetailValidation.TopSensitivityInvalid)
        }
      ).orElse(
        try {
          bottomSensivityText
          None
        } catch {
          case e: IllegalArgumentException => Some(CropDetailValidation.BottomSensitivityInvalid)
          case e: NumberFormatException => Some(CropDetailValidation.BottomSensitivityInvalid)
        }
      )
    case ColorCropCode =>
      (
        try {
          val h = hValue
          if (h < 0 || 360 <= h) Some(CropDetailValidation.HvalueInvalid) else None
        } catch {
          case e: NumberFormatException => Some(CropDetailValidation.HvalueInvalid)
        }
      ).orElse(
        try {
          val s = sValue
          if (s < 0 || 100 < s) Some(CropDetailValidation.SvalueInvalid) else None
        } catch {
          case e: NumberFormatException => Some(CropDetailValidation.SvalueInvalid)
        }
      ).orElse(
        try {
          val v = vValue
          if (v < 0 || 100 < v) Some(CropDetailValidation.VvalueInvalid) else None
        } catch {
          case e: NumberFormatException => Some(CropDetailValidation.VvalueInvalid)
        }
      ).orElse(
        try {
          val e = hsvErrorValue
          if (e < 0 || 100 < e) Some(CropDetailValidation.HsvErrorInvalid) else None
        } catch {
          case e: NumberFormatException => Some(CropDetailValidation.HsvErrorInvalid)
        }
      ).orElse(
        try {
          val t = hsvThresholdValue
          if (t < 0 || 100 < t) Some(CropDetailValidation.HsvThresholdInvalid) else None
        } catch {
          case e: NumberFormatException => Some(CropDetailValidation.HsvThresholdInvalid)
        }
      )
  }
}
