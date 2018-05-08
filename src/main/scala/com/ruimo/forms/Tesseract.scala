package com.ruimo.forms

object Tesseract {
  sealed trait OcrChars
  case object OcrDigit extends OcrChars
  case object OcrUpperAlphabet extends OcrChars
  case object OcrLowerALphabet extends OcrChars
  case object OcrComma extends OcrChars
  case object OcrPeriod extends OcrChars
  case object OcrMinus extends OcrChars
  case object OcrPlus extends OcrChars
  case object OcrSlash extends OcrChars
  case object OcrSharp extends OcrChars
  case object OcrDoller extends OcrChars
  case object OcrBrace extends OcrChars
  case object OcrPercent extends OcrChars
  case object OcrColon extends OcrChars
  case object OcrSemiColon extends OcrChars
  case object OcrEqual extends OcrChars
  case object OcrAsterisc extends OcrChars
  case object OcrAll extends OcrChars

}
