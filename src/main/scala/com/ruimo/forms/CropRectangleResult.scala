package com.ruimo.forms

import play.api.libs.json.JsValue

import scala.collection.{immutable => imm}

sealed trait CropRectangleResult

case class CropRectangleResultSuccess(
  files: imm.Seq[String]
) extends CropRectangleResult

object CropRectangleResult {
  def parse(json: JsValue): CropRectangleResult = {
    CropRectangleResultSuccess(
      files = (json \ "files").as[Seq[String]].toList
    )
  }
}
