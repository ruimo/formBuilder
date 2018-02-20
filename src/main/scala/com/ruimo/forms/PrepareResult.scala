package com.ruimo.forms

import play.api.libs.json.JsValue

case class PrepareResult(
  skewCorrectionResult: Option[SkewCorrectionResult],
  cropResult: Option[CropResult],
  dotRemovalResult: Option[DotRemovalResult],
  cropRectangleResult: Option[CropRectangleResult]
)

object PrepareResult {
  def parse(json: JsValue): PrepareResult = PrepareResult(
    (json \ "skewCorrection").toOption.map { SkewCorrectionResult.parse },
    (json \ "crop").toOption.map { CropResult.parse },
    (json \ "dotRemoval").toOption.map { DotRemovalResult.parse },
    (json \ "cropRectangle").toOption.map { CropRectangleResult.parse }
  )
}
