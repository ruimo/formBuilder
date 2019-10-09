package com.ruimo.forms

import scalafx.scene.control.{CheckBox => SfxCheckBox}
import scalafx.scene.control.{Label => SfxLabel}
import scalafx.scene.control.{TextField => SfxTextField}
import javafx.event.ActionEvent
import scalafx.scene.control.{ComboBox => SfxComboBox}

import scala.collection.{immutable => imm}
import scala.collection.JavaConverters._
import scalafx.collections.ObservableBuffer
import javafx.scene.control._
import javafx.scene.control.TableColumn.CellDataFeatures
import java.net.URL
import java.util.ResourceBundle

import com.ruimo.forms.common._
import com.ruimo.graphics.twodim.Hsv
import com.ruimo.scoins.Percent
import javafx.fxml.{FXML, Initializable}
import javafx.scene.control.TableView
import javafx.util.Callback
import javafx.beans.value.{ChangeListener, ObservableValue}
import javafx.beans.property.ReadOnlyStringWrapper
import javafx.scene.input.{KeyCode, KeyEvent, MouseButton, MouseEvent}
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
case object OcrEngineCodeTegaki extends OcrEngineCode {
  override def toString = "Cogent Tegaki"
}

sealed trait AbsoluteFieldValidationResult {
  def ifOkThen(f: => AbsoluteFieldValidationResult): AbsoluteFieldValidationResult =
    if (this == AbsoluteFieldValidationResult.Ok) f else this
}

object AbsoluteFieldValidationResult {
  case object Ok extends AbsoluteFieldValidationResult
  case object ColorFilterHueInvalid extends AbsoluteFieldValidationResult
  case object ColorFilterHueErrorInvalid extends AbsoluteFieldValidationResult
  case object ColorFilterHueErrorMissing extends AbsoluteFieldValidationResult
  case object BinarizationBrightnessThresholdInvalid extends AbsoluteFieldValidationResult
  
  case class HEdgeThresholdPerHeightInvalid(failure: PercentFieldFailure) extends AbsoluteFieldValidationResult
  case class VEdgeThresholdPerHeightInvalid(failure: PercentFieldFailure) extends AbsoluteFieldValidationResult
  case class AcceptableXgapInvalid(failure: PercentFieldFailure) extends AbsoluteFieldValidationResult
  case class AcceptableYgapInvalid(failure: PercentFieldFailure) extends AbsoluteFieldValidationResult
  case class MinCharBodyWidthPerHeightInvalid(failure: PercentFieldFailure) extends AbsoluteFieldValidationResult
  case class MinCharWidthPerHeightInvalid(failure: PercentFieldFailure) extends AbsoluteFieldValidationResult
  case class MaxCharWidthPerHeightInvalid(failure: PercentFieldFailure) extends AbsoluteFieldValidationResult
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

  @FXML
  private[this] var tegakiGrid: GridPane = _
  lazy val sfxTegakiGrid = new SfxGridPane(tegakiGrid)

  @FXML
  private[this] var tegHiraganaCheck: CheckBox = _
  lazy val sfxTegHiraganaCheck = new SfxCheckBox(tegHiraganaCheck)

  @FXML
  private[this] var tegKatakanaCheck: CheckBox = _
  lazy val sfxTegKatakanaCheck = new SfxCheckBox(tegKatakanaCheck)

  @FXML
  private[this] var tegKanjiCheck: CheckBox = _
  lazy val sfxTegKanjiCheck = new SfxCheckBox(tegKanjiCheck)

  @FXML
  private[this] var tegNumberCheck: CheckBox = _
  lazy val sfxTegNumberCheck = new SfxCheckBox(tegNumberCheck)

  @FXML
  private[this] var tegAlphabetUpperCheck: CheckBox = _
  lazy val sfxTegUpperAlphabetCheck = new SfxCheckBox(tegAlphabetUpperCheck)

  @FXML
  private[this] var tegAlphabetLowerCheck: CheckBox = _
  lazy val sfxTegLowerAlphabetCheck = new SfxCheckBox(tegAlphabetLowerCheck)

  @FXML
  private[this] var tegSymbolCheck: CheckBox = _
  lazy val sfxTegSymbolCheck = new SfxCheckBox(tegSymbolCheck)

  @FXML
  private[this] var tegUseLangMode: CheckBox = _
  lazy val sfxTegUseLangMode = new SfxCheckBox(tegUseLangMode)

  @FXML
  private[this] var tegIsMultiLine: CheckBox = _
  lazy val sfxTegIsMultiLine = new SfxCheckBox(tegIsMultiLine)

  @FXML
  private[this] var colorPassFilterHueText: TextField = _
  lazy val sfxColorPassFilterHueText = new SfxTextField(colorPassFilterHueText)

  @FXML
  private[this] var colorPassFilterHueErrorText: TextField = _
  lazy val sfxColorPassFilterHueErrorText = new SfxTextField(colorPassFilterHueErrorText)

  @FXML
  private[this] var binarizationBrightnessThresholdText: TextField = _
  lazy val sfxBinarizationBrightnessThresholdText = new SfxTextField(binarizationBrightnessThresholdText)

  @FXML
  private[this] var enableMonoSpaceCheck: CheckBox = _
  lazy val sfxEnableMonoSpaceCheck = new SfxCheckBox(enableMonoSpaceCheck)

  @FXML
  private[this] var hEdgeThresholdPerHeightText: TextField = _
  lazy val sfxHEdgeThresholdPerHeightText = new SfxTextField(hEdgeThresholdPerHeightText)

  @FXML
  private[this] var acceptableYgapText: TextField = _
  lazy val sfxAcceptableYgapText = new SfxTextField(acceptableYgapText)

  @FXML
  private[this] var vEdgeThresholdPerHeightText: TextField = _
  lazy val sfxVEdgeThresholdPerHeightText = new SfxTextField(vEdgeThresholdPerHeightText)

  @FXML
  private[this] var acceptableXgapText: TextField = _
  lazy val sfxAcceptableXgapText = new SfxTextField(acceptableXgapText)

  @FXML
  private[this] var minCharBodyWidthPerHeightText: TextField = _
  lazy val sfxMinCharBodyWidthPerHeightText = new SfxTextField(minCharBodyWidthPerHeightText)

  @FXML
  private[this] var minCharWidthPerHeightText: TextField = _
  lazy val sfxMinCharWidthPerHeightText = new SfxTextField(minCharWidthPerHeightText)

  @FXML
  private[this] var maxCharWidthPerHeightText: TextField = _
  lazy val sfxMaxCharWidthPerHeightText = new SfxTextField(maxCharWidthPerHeightText)

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

  def monoSpacedSettings: MonoSpacedSettings = MonoSpacedSettings(
    enabled = sfxEnableMonoSpaceCheck.isSelected(),
    hEdgeThresholdPerHeight = Percent(sfxHEdgeThresholdPerHeightText.text.value.toDouble),
    vEdgeThresholdPerHeight = Percent(sfxVEdgeThresholdPerHeightText.text.value.toDouble),
    acceptableXgap = Percent(sfxAcceptableXgapText.text.value.toDouble),
    acceptableYgap = Percent(sfxAcceptableYgapText.text.value.toDouble),
    minCharBodyWidthPerHeight = Percent(sfxMinCharBodyWidthPerHeightText.text.value.toDouble),
    minCharWidthPerHeight = Percent(sfxMinCharWidthPerHeightText.text.value.toDouble),
    maxCharWidthPerHeight = Percent(sfxMaxCharWidthPerHeightText.text.value.toDouble)
  )

  def applyMonoSpacedSettings(s: MonoSpacedSettings) {
    sfxEnableMonoSpaceCheck.setSelected(s.enabled)
    sfxHEdgeThresholdPerHeightText.text.value = s.hEdgeThresholdPerHeight.value.toString
    sfxVEdgeThresholdPerHeightText.text.value = s.vEdgeThresholdPerHeight.value.toString
    sfxAcceptableXgapText.text.value = s.acceptableXgap.value.toString
    sfxAcceptableYgapText.text.value = s.acceptableYgap.value.toString
    sfxMinCharBodyWidthPerHeightText.text.value = s.minCharBodyWidthPerHeight.value.toString
    sfxMinCharWidthPerHeightText.text.value = s.minCharWidthPerHeight.value.toString
    sfxMaxCharWidthPerHeightText.text.value = s.maxCharWidthPerHeight.value.toString
  }

  def colorPassFilterHue: Option[Double] = {
    val text: String = sfxColorPassFilterHueText.text.value
    if (text.trim.isEmpty) None
    else Some(text.toDouble)
  }

  def colorPassFilterHueError: Double =
    sfxColorPassFilterHueErrorText.text.value.toDouble

  def colorPassFilter: imm.Seq[ColorPassFilterSettings] = colorPassFilterHue.map { h =>
    ColorPassFilterSettings(Hsv.Hue(h), Percent(colorPassFilterHueError))
  }.toList

  def binarization: Option[BinarizationSettings] = {
    val text: String = sfxBinarizationBrightnessThresholdText.text.value
    if (text.trim.isEmpty) None
    else Some(BinarizationSettings(text.toInt))
  }

  def ocrSettings: OcrSettings = sfxOcrEngineComboBox.value() match {
    case OcrEngineCodeGoogle => GoogleOcrSettings(
      colorPassFilter = colorPassFilter,
      binarization = binarization,
      lang = sfxGoogleLangComboBox.value()
    )

    case OcrEngineCodeTegaki => TegakiOcrSettings(
      colorPassFilter = colorPassFilter,
      binarization = binarization,
      useLangModel = sfxTegUseLangMode.isSelected,
      isMultiLine =  sfxTegIsMultiLine.isSelected,
      acceptChars =  TegakiAcceptChars(
        isHiragana = sfxTegHiraganaCheck.isSelected,
        isKatakan = sfxTegKatakanaCheck.isSelected,
        isKanji = sfxTegKanjiCheck.isSelected,
        isNumber = sfxTegNumberCheck.isSelected,
        isUpperAlpha = sfxTegUpperAlphabetCheck.isSelected,
        isLowerAlpha = sfxTegLowerAlphabetCheck.isSelected,
        isSymbol = sfxTegSymbolCheck.isSelected
      )
    )

    case _ => TesseractOcrSettings(
      colorPassFilter = colorPassFilter,
      binarization = binarization,
      lang = sfxTesLangDropDown.value(),
      acceptChars = TesseractAcceptChars(
        tesseractAcceptChars,
        sfxTesCustomChar.text()
      ),
      monoSpacedSettings = MonoSpacedSettings(
        enabled = sfxEnableMonoSpaceCheck.selected.value,
        hEdgeThresholdPerHeight = Percent(sfxHEdgeThresholdPerHeightText.text.value.toDouble),
        vEdgeThresholdPerHeight = Percent(sfxVEdgeThresholdPerHeightText.text.value.toDouble),
        acceptableXgap = Percent(sfxAcceptableXgapText.text.value.toDouble),
        acceptableYgap = Percent(sfxAcceptableYgapText.text.value.toDouble),
        minCharBodyWidthPerHeight = Percent(sfxMinCharBodyWidthPerHeightText.text.value.toDouble),
        minCharWidthPerHeight = Percent(sfxMinCharWidthPerHeightText.text.value.toDouble),
        maxCharWidthPerHeight = Percent(sfxMaxCharWidthPerHeightText.text.value.toDouble)
      )
    )
  }

  def setColorPassFilter(settings: imm.Seq[ColorPassFilterSettings]) {
    settings.headOption match {
      case None =>
        sfxColorPassFilterHueText.text.value = ""
        sfxColorPassFilterHueErrorText.text.value = ""

      case Some(cpfs) =>
        sfxColorPassFilterHueText.text.value = cpfs.hueValue.value.toString
        sfxColorPassFilterHueErrorText.text.value = cpfs.hueErrorAllowance.value.toString
    }
  }

  def setBinarization(settings: Option[BinarizationSettings]): Unit = {
    settings match {
      case None =>
        sfxBinarizationBrightnessThresholdText.text.value = ""
      case Some(s) =>
        sfxBinarizationBrightnessThresholdText.text.value = s.brightnessThreshold.toString
    }
  }

  def ocrSettings_=(newOcrSettings: OcrSettings) {
    setColorPassFilter(newOcrSettings.colorPassFilter)
    setBinarization(newOcrSettings.binarization)
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
        sfxEnableMonoSpaceCheck.selected.value = tos.monoSpacedSettings.enabled
        sfxHEdgeThresholdPerHeightText.text.value = tos.monoSpacedSettings.hEdgeThresholdPerHeight.value.toString
        sfxVEdgeThresholdPerHeightText.text.value = tos.monoSpacedSettings.vEdgeThresholdPerHeight.value.toString
        sfxAcceptableXgapText.text.value = tos.monoSpacedSettings.acceptableXgap.value.toString
        sfxAcceptableYgapText.text.value = tos.monoSpacedSettings.acceptableYgap.value.toString
        sfxMinCharBodyWidthPerHeightText.text.value = tos.monoSpacedSettings.minCharBodyWidthPerHeight.value.toString
        sfxMinCharWidthPerHeightText.text.value = tos.monoSpacedSettings.minCharWidthPerHeight.value.toString
        sfxMaxCharWidthPerHeightText.text.value = tos.monoSpacedSettings.maxCharWidthPerHeight.value.toString
        selectOcrPane(OcrEngineCodeTesseract)

      case teg: TegakiOcrSettings =>
        sfxTegUseLangMode.setSelected(teg.useLangModel)
        sfxTegIsMultiLine.setSelected(teg.isMultiLine)
        sfxTegHiraganaCheck.setSelected(teg.acceptChars.isHiragana)
        sfxTegKatakanaCheck.setSelected(teg.acceptChars.isKatakan)
    }
  }

  def gridPane(oec: OcrEngineCode): SfxGridPane = {
    oec match {
      case OcrEngineCodeTesseract => sfxTesseractGrid
      case OcrEngineCodeGoogle => sfxGoogleGrid

      case OcrEngineCodeMicrosoft => sfxTesseractGrid
      case OcrEngineCodeTegaki => sfxTegakiGrid
    }
  }

  def selectOcrPane(ocrEngine: OcrEngineCode) {
    sfxTesseractGrid.visible = false
    sfxGoogleGrid.visible = false
    sfxTegakiGrid.visible = false

    gridPane(ocrEngine).visible = true
  }

  override def initialize(url: URL, resourceBundle: ResourceBundle) {
    logger.info("AbsoluteFieldController initialize")
    sfxOcrEngineComboBox += OcrEngineCodeTesseract
    sfxOcrEngineComboBox += OcrEngineCodeGoogle
    sfxOcrEngineComboBox += OcrEngineCodeMicrosoft
    sfxOcrEngineComboBox += OcrEngineCodeTegaki
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

  def validate: AbsoluteFieldValidationResult =
    validateColorFilter.ifOkThen {
      validateBinarization
    }.ifOkThen {
      validateMonoSpacedSettings
    }

  def validateColorFilter: AbsoluteFieldValidationResult = {
    try {
      colorPassFilterHue match {
        case None => AbsoluteFieldValidationResult.Ok
        case Some(h) =>
          if (0 <= h && h < 360) {
            val s = sfxColorPassFilterHueErrorText.text.value
            if (s.trim.isEmpty) AbsoluteFieldValidationResult.ColorFilterHueErrorMissing
            else {
              try {
                val e = s.toDouble
                if (0 <= e && e <= 100) AbsoluteFieldValidationResult.Ok
                else AbsoluteFieldValidationResult.ColorFilterHueErrorInvalid
              } catch {
                case err: NumberFormatException => AbsoluteFieldValidationResult.ColorFilterHueErrorInvalid
              }
            }
          } else
              AbsoluteFieldValidationResult.ColorFilterHueInvalid
      }
    } catch {
      case e: NumberFormatException =>
        AbsoluteFieldValidationResult.ColorFilterHueInvalid
    }
  }

  def validateBinarization: AbsoluteFieldValidationResult = {
    try {
      binarization match {
        case None => AbsoluteFieldValidationResult.Ok
        case Some(s) =>
          if (0 <= s.brightnessThreshold && s.brightnessThreshold <= 255)
            AbsoluteFieldValidationResult.Ok
          else
            AbsoluteFieldValidationResult.BinarizationBrightnessThresholdInvalid
       }
    } catch {
      case e: NumberFormatException =>
        AbsoluteFieldValidationResult.BinarizationBrightnessThresholdInvalid
    }
  }

  def parseTextField(
    textField: SfxTextField, f: PercentFieldFailure => AbsoluteFieldValidationResult,
    min: Percent = Percent(0), max: Percent = Percent(100)
  ): AbsoluteFieldValidationResult = {
    import PercentFieldResult.{parseTextField => parse}

    parse(textField, Some(min), Some(max)) match {
      case Left(fail) => f(fail)
      case Right(p) => AbsoluteFieldValidationResult.Ok
    }
  }

  def validateMonoSpacedSettings: AbsoluteFieldValidationResult = {
    if (sfxEnableMonoSpaceCheck.selected.value) {
      import AbsoluteFieldValidationResult._

      parseTextField(sfxHEdgeThresholdPerHeightText, HEdgeThresholdPerHeightInvalid.apply).ifOkThen {
        parseTextField(sfxVEdgeThresholdPerHeightText, VEdgeThresholdPerHeightInvalid.apply)
      }.ifOkThen {
        parseTextField(sfxAcceptableXgapText, AcceptableXgapInvalid.apply)
      }.ifOkThen {
        parseTextField(sfxAcceptableYgapText, AcceptableYgapInvalid.apply)
      }.ifOkThen {
        parseTextField(sfxMinCharBodyWidthPerHeightText, MinCharBodyWidthPerHeightInvalid.apply)
      }.ifOkThen {
        parseTextField(sfxMinCharWidthPerHeightText, MinCharWidthPerHeightInvalid.apply)
      }.ifOkThen {
        parseTextField(sfxMaxCharWidthPerHeightText, MaxCharWidthPerHeightInvalid.apply, max = Percent(500))
      }
    } else {
      AbsoluteFieldValidationResult.Ok
    }
  }

  def fillEmptyMonoSpacedSettings(): Unit = {
    def fill(textField: SfxTextField, default: Percent) {
      if (textField.text.value.trim.isEmpty()) {
        textField.text.value = default.value.toString
      }
    }

    fill(sfxHEdgeThresholdPerHeightText, MonoSpacedSettings.DefaultHEdgeThresholdPerHeight)
    fill(sfxVEdgeThresholdPerHeightText, MonoSpacedSettings.DefaultVEdgeThresholdPerHeight)
    fill(sfxAcceptableXgapText, MonoSpacedSettings.DefaultAcceptableXgap)
    fill(sfxAcceptableYgapText, MonoSpacedSettings.DefaultAcceptableYgap)
    fill(sfxMinCharBodyWidthPerHeightText, MonoSpacedSettings.DefaultMinCharBodyWidthPerHeight)
    fill(sfxMinCharWidthPerHeightText, MonoSpacedSettings.DefaultMinCharWidthPerHeight)
    fill(sfxMaxCharWidthPerHeightText, MonoSpacedSettings.DefaultMaxCharWidthPerHeight)
  }

  @FXML
  def onResetMonoSpacedSettings(e: ActionEvent) {
    sfxHEdgeThresholdPerHeightText.text.value = ""
    sfxVEdgeThresholdPerHeightText.text.value = ""
    sfxAcceptableXgapText.text.value = ""
    sfxAcceptableYgapText.text.value = ""
    sfxMinCharBodyWidthPerHeightText.text.value = ""
    sfxMinCharWidthPerHeightText.text.value = ""
    sfxMaxCharWidthPerHeightText.text.value = ""
    fillEmptyMonoSpacedSettings()
  }
}
