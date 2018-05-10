package com.ruimo.forms

import scalafx.scene.control.{CheckBox => SfxCheckBox}
import scalafx.scene.control.{Label => SfxLabel}
import scalafx.scene.control.{TextField => SfxTextField}
import javafx.event.ActionEvent
import scalafx.scene.control.{ComboBox => SfxComboBox}
import scala.collection.JavaConverters._
import scalafx.collections.ObservableBuffer
import javafx.scene.control._
import javafx.scene.control.TableColumn.CellDataFeatures
import java.net.URL
import java.util.ResourceBundle
import javafx.fxml.{FXML, Initializable}
import javafx.scene.control.TableView
import javafx.util.Callback
import javafx.beans.value.ObservableValue
import javafx.beans.property.ReadOnlyStringWrapper
import javafx.scene.input.{KeyCode, KeyEvent, MouseButton, MouseEvent}

sealed trait OcrEngineCode
case object OcrEngineCodeTesseract extends OcrEngineCode {
  override def toString = "Tesseract"
}
case object OcrEngineCodeGoogle extends OcrEngineCode {
  override def toString = "Google OCR"
}
case object OcrEngineCodeMicrosoft extends OcrEngineCode {
  override def toString = "Microsoft OCR"
}
case object OcrEngineCodeCogent extends OcrEngineCode {
  override def toString = "Cogent Tegaki"
}

class AbsoluteFieldController extends Initializable {
  @FXML
  private[this] var fieldNameText: TextField = _

  @FXML
  private[this] var ocrEngineComboBox: ComboBox[OcrEngineCode] = _
  lazy val sfxOcrEngineComboBox = new SfxComboBox[OcrEngineCode](ocrEngineComboBox)

  @FXML
  private[this] var tesCharsDropDown: ComboBox[String] = _
  lazy val sfxTesCharsDropDown = new SfxComboBox[String](tesCharsDropDown)

  @FXML
  private[this] var tesLangDropDown: ComboBox[TesseractLang] = _
  lazy val sfxTesLangDropDown = new SfxComboBox[TesseractLang](tesLangDropDown)

  @FXML
  private[this] var tesDigitCheck: CheckBox = _
  lazy val sfxTesDigitCheck = new SfxCheckBox(tesDigitCheck)

  @FXML
  private[this] var tesUpperAlphCheck: CheckBox = _
  lazy val sfxTesUpperAlphCheck = new SfxCheckBox(tesUpperAlphCheck)

  @FXML
  private[this] var tesLowerAlphCheck: CheckBox = _
  lazy val sfxTesLowerAlphCheck = new SfxCheckBox(tesLowerAlphCheck)

  @FXML
  private[this] var tesCommaCheck: CheckBox = _
  lazy val sfxTesCommaCheck = new SfxCheckBox(tesCommaCheck)

  @FXML
  private[this] var tesPeriodCheck: CheckBox = _
  lazy val sfxTesPeriodCheck = new SfxCheckBox(tesPeriodCheck)

  @FXML
  private[this] var tesMinusCheck: CheckBox = _
  lazy val sfxTesMinusCheck = new SfxCheckBox(tesMinusCheck)

  @FXML
  private[this] var tesPlusCheck: CheckBox = _
  lazy val sfxTesPlusCheck = new SfxCheckBox(tesPlusCheck)

  @FXML
  private[this] var tesSlashCheck: CheckBox = _
  lazy val sfxTesSlashCheck = new SfxCheckBox(tesSlashCheck)

  @FXML
  private[this] var tesSharpCheck: CheckBox = _
  lazy val sfxTesSharpCheck = new SfxCheckBox(tesSharpCheck)

  @FXML
  private[this] var tesDollerCheck: CheckBox = _
  lazy val sfxTesDollerCheck = new SfxCheckBox(tesDollerCheck)

  @FXML
  private[this] var tesBraceCheck: CheckBox = _
  lazy val sfxTesBraceCheck = new SfxCheckBox(tesBraceCheck)

  @FXML
  private[this] var tesPercentCheck: CheckBox = _
  lazy val sfxTesPercentCheck = new SfxCheckBox(tesPercentCheck)

  @FXML
  private[this] var tesColonCheck: CheckBox = _
  lazy val sfxTesColonCheck = new SfxCheckBox(tesColonCheck)

  @FXML
  private[this] var tesSemiColonCheck: CheckBox = _
  lazy val sfxTesSemiColonCheck = new SfxCheckBox(tesSemiColonCheck)

  @FXML
  private[this] var tesEqualCheck: CheckBox = _
  lazy val sfxTesEqualCheck = new SfxCheckBox(tesEqualCheck)

  @FXML
  private[this] var tesAsteriskCheck: CheckBox = _
  lazy val sfxTesAsteriskCheck = new SfxCheckBox(tesAsteriskCheck)

  @FXML
  private[this] var tesAllCheck: CheckBox = _
  lazy val sfxTesAllCheck = new SfxCheckBox(tesAllCheck)

  @FXML
  private[this] var tesCustomChar: TextField = _
  lazy val sfxTesCustomChar = new SfxTextField(tesCustomChar)

  @FXML
  private[this] var tesCustomLabel: Label = _
  lazy val sfxTesCustomLabel = new SfxLabel(tesCustomLabel)

  @FXML
  def tesAllCheckClicked(e: ActionEvent) {
    println("All clicked: " + sfxTesAllCheck.selected())
    def onAllChecked(isChecked: Boolean) {
      sfxTesCustomChar.visible = !isChecked
      sfxTesCustomLabel.visible = !isChecked
      sfxTesDigitCheck.visible = !isChecked
      sfxTesUpperAlphCheck.visible = !isChecked
      sfxTesLowerAlphCheck.visible = !isChecked
      sfxTesCommaCheck.visible = !isChecked
      sfxTesPeriodCheck.visible = !isChecked
      sfxTesMinusCheck.visible = !isChecked
      sfxTesPlusCheck.visible = !isChecked
      sfxTesSlashCheck.visible = !isChecked
      sfxTesSharpCheck.visible = !isChecked
      sfxTesDollerCheck.visible = !isChecked
      sfxTesBraceCheck.visible = !isChecked
      sfxTesPercentCheck.visible = !isChecked
      sfxTesColonCheck.visible = !isChecked
      sfxTesSemiColonCheck.visible = !isChecked
      sfxTesEqualCheck.visible = !isChecked
      sfxTesAsteriskCheck.visible = !isChecked
    }

    onAllChecked(sfxTesAllCheck.selected())
  }

  def fieldName: String = fieldNameText.getText()

  def fieldName_=(newName: String) {
    fieldNameText.setText(newName)
  }

  override def initialize(url: URL, resourceBundle: ResourceBundle) {
    println("AbsoluteFieldController initialize")
    sfxOcrEngineComboBox += OcrEngineCodeTesseract
    sfxOcrEngineComboBox += OcrEngineCodeGoogle
    sfxOcrEngineComboBox += OcrEngineCodeMicrosoft
    sfxOcrEngineComboBox += OcrEngineCodeCogent
    sfxOcrEngineComboBox.value = OcrEngineCodeTesseract

    sfxTesLangDropDown += TesseractLangJa
    sfxTesLangDropDown += TesseractLangEn
    sfxTesLangDropDown.value = TesseractLangEn
  }
}
