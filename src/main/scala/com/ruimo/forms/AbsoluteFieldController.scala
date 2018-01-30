package com.ruimo.forms

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
