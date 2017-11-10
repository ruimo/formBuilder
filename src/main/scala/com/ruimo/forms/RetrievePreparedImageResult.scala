package com.ruimo.forms

import scalafx.scene.image.Image

sealed trait RetrievePreparedImageResult
sealed trait RetrievePreparedImageFailure extends RetrievePreparedImageResult

case class RetrievePreparedImageAuthError(
  statusCode: Int,
  statusText: String,
  body: String
) extends RetrievePreparedImageFailure

case class RetrievePreparedImageUnknownError(
  statusCode: Int,
  statusText: String,
  body: String
) extends RetrievePreparedImageFailure

class RetrievePreparedImageResultOk(
  val serverResp: Option[PrepareResult],
  val image: Image
) extends RetrievePreparedImageResult
