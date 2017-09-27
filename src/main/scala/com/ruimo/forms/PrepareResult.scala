package com.ruimo.forms

import play.api.libs.json.JsValue

case class PrepareResult(
  skewCorrectionResult: Option[SkewCorrectionResult],
  cropResult: Option[CropResult]
)

object PrepareResult {
  def parse(json: JsValue): PrepareResult = PrepareResult(
    (json \ "skewCorrection").toOption.map { SkewCorrectionResult.parse },
    (json \ "crop").toOption.map { CropResult.parse }
  )
}
