package com.ruimo.forms

import scalafx.scene.image.Image

sealed trait RetrievePreparedImageRestResult
sealed trait RetrievePreparedImageRestFailure extends RetrievePreparedImageRestResult

sealed trait CaptureRestResult
sealed trait CaptureRestFailure extends CaptureRestResult

case class RestAuthFailure(
  statusCode: Int,
  statusText: String,
  body: String
) extends RetrievePreparedImageRestFailure with CaptureRestFailure

case class RestUnknownFailure(
  statusCode: Int,
  statusText: String,
  body: String
) extends RetrievePreparedImageRestFailure with CaptureRestFailure

class RetrievePreparedImageResultOk(
  val serverResp: Option[PrepareResult],
  val image: Image
) extends RetrievePreparedImageRestResult

class CaptureResultOk(
  val serverResp: Option[PrepareResult],
  val image: Image
) extends CaptureRestResult
