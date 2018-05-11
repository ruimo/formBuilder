package com.ruimo.forms

import javafx.fxml.{FXML, Initializable}
import java.net.URL
import java.util.ResourceBundle

import javafx.scene.control.{ComboBox, TextField}
import com.ruimo.scoins.Percent
import org.slf4j.LoggerFactory
import scalafx.collections.ObservableBuffer
import scalafx.scene.control.{ComboBox => SfxComboBox}

class SkewCorrectionDetailController extends Initializable with HasLogger {
  val HorizontalLineDetection = "横線検出"
  val VerticalLineDetection = "縦線検出"

  @FXML
  private[this] var hvComboBox: ComboBox[String] = _

  @FXML
  private[this] var lineCountText: TextField = _

  @FXML
  private[this] var maxAngleText: TextField = _

  @FXML
  private[this] var detectAreaFromText: TextField = _

  @FXML
  private[this] var detectAreaToText: TextField = _

  lazy val sfxHvComboBox = new SfxComboBox[String](hvComboBox)

  override def initialize(url: URL, resourceBundle: ResourceBundle) {
    logger.info("SkewCorrectionDetailController initialize")
    sfxHvComboBox += HorizontalLineDetection
    sfxHvComboBox += VerticalLineDetection
    sfxHvComboBox.value = HorizontalLineDetection
  }

  def model: SkewCorrectionCondition = SkewCorrectionConditionImpl(
    direction =
      if (sfxHvComboBox.value() == HorizontalLineDetection) SkewCorrectionDirectionHorizontal
      else SkewCorrectionDirectionVertical,
    lineCount = lineCountText.getText().toInt,
    maxAngleToDetect = maxAngleText.getText().toDouble,
    detectAreaFrom = Percent(detectAreaFromText.getText.toDouble),
    detectAreaTo = Percent(detectAreaToText.getText.toDouble)
  )

  def model_=(newModel: SkewCorrectionCondition) {
    val dir = if (newModel.direction == SkewCorrectionDirectionHorizontal)
      HorizontalLineDetection
    else
      VerticalLineDetection

    sfxHvComboBox.value = dir
    lineCountText.setText(newModel.lineCount.toString)
    maxAngleText.setText(newModel.maxAngleToDetect.toString)
    detectAreaFromText.setText(newModel.detectAreaFrom.value.toString)
    detectAreaToText.setText(newModel.detectAreaTo.value.toString)
  }
}
