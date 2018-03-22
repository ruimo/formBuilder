package com.ruimo.forms

import play.api.libs.json.JsValue

import scala.collection.{immutable => imm}

sealed trait RemoveRuledLineResult

case class RemoveRuledLineResultSuccess(
  files: imm.Seq[String]
) extends RemoveRuledLineResult

object RemoveRuledLineResult {
  def parse(json: JsValue): RemoveRuledLineResult = {
    RemoveRuledLineResultSuccess(
      files = (json \ "files").as[Seq[String]].toList
    )
  }
}
