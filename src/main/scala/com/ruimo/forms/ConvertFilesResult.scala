package com.ruimo.forms

import java.io.File

import scalafx.scene.image.Image
import scala.collection.{immutable => imm}
import java.nio.file.Path

sealed trait ConvertFilesResult
sealed trait ConvertFilesResultFailure extends ConvertFilesResult

case class ConvertFilesAuthError(
  statusCode: Int,
  statusText: String,
  body: String
) extends ConvertFilesResultFailure

case class ConvertFilesUnknownError(
  statusCode: Int,
  statusText: String,
  body: String,
  path: Path
) extends ConvertFilesResultFailure

class ConvertFilesResultOk(
  val files: imm.Seq[File]
) extends ConvertFilesResult
