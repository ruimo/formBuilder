package com.ruimo.forms

import play.api.libs.json.JsValue

import scala.collection.{immutable => imm}

sealed trait CropResult

case class CropResultSuccess(
  correctedFiles: imm.Seq[String]
) extends CropResult

case object CropResultCannotFindEdge extends CropResult

object CropResult {
  def parse(json: JsValue): CropResult = {
    if ((json \ "status").as[String] == "Success")
      CropResultSuccess(
        correctedFiles = (json \ "correctedFiles").as[Seq[String]].toList
      )
    else CropResultCannotFindEdge
  }
}
