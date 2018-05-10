package com.ruimo.forms

import scala.collection.{immutable => imm}

object Tesseract {
  sealed trait OcrChars {
    val code: String
  }
  object OcrChars {
    def tryParse(s: String): Option[OcrChars] = mapFromString.get(s)
  }
  case object OcrDigit extends OcrChars {
    val code = "0-9"
  }
  case object OcrUpperAlphabet extends OcrChars {
    val code = "A-Z"
  }
  case object OcrLowerALphabet extends OcrChars {
    val code = "a-z"
  }
  case object OcrComma extends OcrChars {
    val code = ","
  }
  case object OcrPeriod extends OcrChars {
    val code = "."
  }
  case object OcrMinus extends OcrChars {
    val code = "+"
  }
  case object OcrPlus extends OcrChars {
    val code = "+"
  }
  case object OcrSlash extends OcrChars {
    val code = "/"
  }
  case object OcrSharp extends OcrChars {
    val code = "#"
  }
  case object OcrDoller extends OcrChars {
    val code = "$"
  }
  case object OcrBrace extends OcrChars {
    val code = "()"
  }
  case object OcrPercent extends OcrChars {
    val code = "%"
  }
  case object OcrColon extends OcrChars {
    val code = ":"
  }
  case object OcrSemiColon extends OcrChars {
    val code = ";"
  }
  case object OcrEqual extends OcrChars {
    val code = "="
  }
  case object OcrAsterisc extends OcrChars {
    val code = "*"
  }

  private[this] val mapFromString: imm.Map[String, OcrChars] = imm.HashMap(
    "0-9" -> OcrDigit,
    "A-Z" -> OcrUpperAlphabet,
    "a-z" -> OcrLowerALphabet,
    "," -> OcrComma,
    "." -> OcrPeriod,
    "-" -> OcrMinus,
    "+" -> OcrPlus,
    "/" -> OcrSlash,
    "#" -> OcrSharp,
    "$" -> OcrDoller,
    "()" -> OcrBrace,
    "%" -> OcrPercent,
    ":" -> OcrColon,
    ";" -> OcrSemiColon,
    "=" -> OcrEqual,
    "*" -> OcrAsterisc
  )
}
