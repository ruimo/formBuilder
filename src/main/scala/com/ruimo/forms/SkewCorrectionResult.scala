package com.ruimo.forms

import play.api.libs.json.JsValue

import scala.collection.{immutable => imm}

case class SkewCorrectionFoundLine(
  ro: Double, theta: Double
)

object SkewCorrectionFoundLine {
  def parse(json: JsValue): SkewCorrectionFoundLine =
    SkewCorrectionFoundLine(
      (json \ "ro").as[Double],
      (json \ "th").as[Double]
    )
}

case class SkewCorrectionResult(
  foundLines: imm.Seq[SkewCorrectionFoundLine],
  correctedFiles: imm.Seq[String]
)

object SkewCorrectionResult {
  val Null: SkewCorrectionResult = {
    SkewCorrectionResult(
      imm.Seq(), imm.Seq()
    )
  }

  def parse(json: JsValue): SkewCorrectionResult = SkewCorrectionResult(
    foundLines = (json \ "foundLines").as[Seq[JsValue]].map(SkewCorrectionFoundLine.parse).toList,
    correctedFiles = (json \ "correctedFiles").as[Seq[String]].toList
  )
}
