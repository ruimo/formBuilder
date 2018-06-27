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
import javafx.beans.value.{ChangeListener, ObservableValue}
import javafx.beans.property.ReadOnlyStringWrapper
import javafx.scene.input.{KeyCode, KeyEvent, MouseButton, MouseEvent}
import com.ruimo.forms.common.{TesseractAcceptChars, _}
import javafx.scene.layout.GridPane
import scalafx.scene.layout.{GridPane => SfxGridPane}
import org.slf4j.LoggerFactory

import scala.collection.{immutable => imm}

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

class AbsoluteFieldController extends Initializable with HasLogger {
  @FXML
  private[this] var fieldNameText: TextField = _

  @FXML
  private[this] var ocrEngineComboBox: ComboBox[OcrEngineCode] = _
  lazy val sfxOcrEngineComboBox = new SfxComboBox[OcrEngineCode](ocrEngineComboBox)

  @FXML
  private[this] var googleLangComboBox: ComboBox[GoogleOcrLang] = _
  lazy val sfxGoogleLangComboBox = new SfxComboBox[GoogleOcrLang](googleLangComboBox)

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
  private[this] var tesCustomChar: TextField = _
  lazy val sfxTesCustomChar = new SfxTextField(tesCustomChar)

  @FXML
  private[this] var tesCustomLabel: Label = _
  lazy val sfxTesCustomLabel = new SfxLabel(tesCustomLabel)

  @FXML
  private[this] var tesseractGrid: GridPane = _
  lazy val sfxTesseractGrid = new SfxGridPane(tesseractGrid)

  @FXML
  private[this] var googleGrid: GridPane = _
  lazy val sfxGoogleGrid = new SfxGridPane(googleGrid)

  def fieldName: String = fieldNameText.getText()

  def fieldName_=(newName: String) {
    fieldNameText.setText(newName)
  }

  def tesseractAcceptChars: imm.Set[Tesseract.OcrChars] = {
    var chars = imm.Set[Tesseract.OcrChars]()
    if (sfxTesDigitCheck.isSelected()) chars = chars + Tesseract.OcrDigit
    if (sfxTesUpperAlphCheck.isSelected()) chars = chars + Tesseract.OcrUpperAlphabet
    if (sfxTesLowerAlphCheck.isSelected()) chars = chars + Tesseract.OcrLowerALphabet
    if (sfxTesCommaCheck.isSelected()) chars = chars + Tesseract.OcrComma
    if (sfxTesPeriodCheck.isSelected()) chars = chars + Tesseract.OcrPeriod
    if (sfxTesMinusCheck.isSelected()) chars = chars + Tesseract.OcrMinus
    if (sfxTesPlusCheck.isSelected()) chars = chars + Tesseract.OcrPlus
    if (sfxTesSlashCheck.isSelected()) chars = chars + Tesseract.OcrSlash
    if (sfxTesSharpCheck.isSelected()) chars = chars + Tesseract.OcrSharp
    if (sfxTesDollerCheck.isSelected()) chars = chars + Tesseract.OcrDoller
    if (sfxTesBraceCheck.isSelected()) chars = chars + Tesseract.OcrBrace
    if (sfxTesPercentCheck.isSelected()) chars = chars + Tesseract.OcrPercent
    if (sfxTesColonCheck.isSelected()) chars = chars + Tesseract.OcrColon
    if (sfxTesSemiColonCheck.isSelected()) chars = chars + Tesseract.OcrSemiColon
    if (sfxTesEqualCheck.isSelected()) chars = chars + Tesseract.OcrEqual
    if (sfxTesAsteriskCheck.isSelected()) chars = chars + Tesseract.OcrAsterisc
    chars
  }

  def applyTesseractAcceptChars(chars: Set[Tesseract.OcrChars]) {
    sfxTesDigitCheck.selected = chars.contains(Tesseract.OcrDigit)
    sfxTesUpperAlphCheck.selected = chars.contains(Tesseract.OcrUpperAlphabet)
    sfxTesLowerAlphCheck.selected = chars.contains(Tesseract.OcrLowerALphabet)
    sfxTesCommaCheck.selected = chars.contains(Tesseract.OcrComma)
    sfxTesPeriodCheck.selected = chars.contains(Tesseract.OcrPeriod)
    sfxTesMinusCheck.selected = chars.contains(Tesseract.OcrMinus)
    sfxTesPlusCheck.selected = chars.contains(Tesseract.OcrPlus)
    sfxTesSlashCheck.selected = chars.contains(Tesseract.OcrSlash)
    sfxTesSharpCheck.selected = chars.contains(Tesseract.OcrSharp)
    sfxTesDollerCheck.selected = chars.contains(Tesseract.OcrDoller)
    sfxTesBraceCheck.selected = chars.contains(Tesseract.OcrBrace)
    sfxTesPercentCheck.selected = chars.contains(Tesseract.OcrPercent)
    sfxTesColonCheck.selected = chars.contains(Tesseract.OcrColon)
    sfxTesSemiColonCheck.selected = chars.contains(Tesseract.OcrSemiColon)
    sfxTesEqualCheck.selected = chars.contains(Tesseract.OcrEqual)
    sfxTesAsteriskCheck.selected = chars.contains(Tesseract.OcrAsterisc)
  }

  def ocrSettings: OcrSettings = sfxOcrEngineComboBox.value() match {
    case OcrEngineCodeGoogle => GoogleOcrSettings(
      lang = sfxGoogleLangComboBox.value()
    )

    case _ => TesseractOcrSettings(
      lang = sfxTesLangDropDown.value(),
      acceptChars = TesseractAcceptChars(
        tesseractAcceptChars,
        sfxTesCustomChar.text()
      )
    )
  }

  def ocrSettings_=(newOcrSettings: OcrSettings) {
    newOcrSettings match {
      case gos: GoogleOcrSettings =>
        sfxOcrEngineComboBox.value = OcrEngineCodeGoogle
        sfxGoogleLangComboBox.value = gos.lang
        selectOcrPane(OcrEngineCodeGoogle)

      case tos: TesseractOcrSettings =>
        sfxOcrEngineComboBox.value = OcrEngineCodeTesseract
        sfxTesLangDropDown.value = tos.lang
        tos.acceptChars match {
          case ta: TesseractAcceptChars =>
            applyTesseractAcceptChars(ta.chars)
            sfxTesCustomChar.text = ta.custom
          case _ =>
        }
        selectOcrPane(OcrEngineCodeTesseract)
    }
  }

  def gridPane(oec: OcrEngineCode): SfxGridPane = {
    oec match {
      case OcrEngineCodeTesseract => sfxTesseractGrid
      case OcrEngineCodeGoogle => sfxGoogleGrid

      case OcrEngineCodeMicrosoft => sfxTesseractGrid
      case OcrEngineCodeCogent => sfxTesseractGrid
    }
  }

  def selectOcrPane(ocrEngine: OcrEngineCode) {
    sfxTesseractGrid.visible = false
    sfxGoogleGrid.visible = false

    gridPane(ocrEngine).visible = true
  }

  override def initialize(url: URL, resourceBundle: ResourceBundle) {
    logger.info("AbsoluteFieldController initialize")
    sfxOcrEngineComboBox += OcrEngineCodeTesseract
    sfxOcrEngineComboBox += OcrEngineCodeGoogle
    sfxOcrEngineComboBox += OcrEngineCodeMicrosoft
    sfxOcrEngineComboBox += OcrEngineCodeCogent
    sfxOcrEngineComboBox.value = OcrEngineCodeTesseract

    selectOcrPane(OcrEngineCodeTesseract)

    sfxTesLangDropDown += TesseractLangJa
    sfxTesLangDropDown += TesseractLangEn
    sfxTesLangDropDown.value = TesseractLangEn

    sfxGoogleLangComboBox += GoogleOcrLangJa
    sfxGoogleLangComboBox += GoogleOcrLangEn
    sfxGoogleLangComboBox.value = GoogleOcrLangEn

    sfxOcrEngineComboBox.valueProperty.addListener(new ChangeListener[OcrEngineCode] {
      override def changed(observableValue: ObservableValue[_ <: OcrEngineCode], from: OcrEngineCode, to: OcrEngineCode) {
        gridPane(from).setVisible(false)
        gridPane(to).setVisible(true)
      }
    })
  }
}
