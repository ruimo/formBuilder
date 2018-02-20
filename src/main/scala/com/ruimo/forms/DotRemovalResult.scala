package com.ruimo.forms

import play.api.libs.json.JsValue

import scala.collection.{immutable => imm}

sealed trait DotRemovalResult

case class DotRemovalResultSuccess(
  files: imm.Seq[String]
) extends DotRemovalResult

object DotRemovalResult {
  def parse(json: JsValue): DotRemovalResult = {
    DotRemovalResultSuccess(
      files = (json \ "files").as[Seq[String]].toList
    )
  }
}
