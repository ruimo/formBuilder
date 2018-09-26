package com.ruimo.forms

import java.time.Instant

import play.api.libs.json.JsValue

import scala.collection.{immutable => imm}
import scalafx.scene.image.Image

sealed trait RetrievePreparedImageRestResult
sealed trait RetrievePreparedImageRestFailure extends RetrievePreparedImageRestResult

sealed trait CaptureRestResult
sealed trait CaptureRestFailure extends CaptureRestResult

sealed trait SaveConfigRestResult
sealed trait SaveConfigRestFailure extends SaveConfigRestResult

sealed trait OpenConfigRestResult
sealed trait OpenConfigRestFailure extends OpenConfigRestResult

sealed trait ListConfigRestResult
sealed trait ListConfigRestFailure extends ListConfigRestResult

sealed trait RemoveConfigRestResult
sealed trait RemoveConfigRestFailure extends RemoveConfigRestResult

case class RestAuthFailure(
  statusCode: Int,
  statusText: String,
  body: String
) extends RetrievePreparedImageRestFailure
    with CaptureRestFailure with SaveConfigRestFailure with ListConfigRestFailure with RemoveConfigRestFailure
    with OpenConfigRestFailure

case class RestSizeError(
  statusCode: Int,
  statusText: String
) extends RetrievePreparedImageRestFailure
    with CaptureRestFailure

case class RestUnknownFailure(
  statusCode: Int,
  statusText: String,
  body: String
) extends RetrievePreparedImageRestFailure
    with CaptureRestFailure with SaveConfigRestFailure with ListConfigRestFailure with RemoveConfigRestFailure
    with OpenConfigRestFailure

case class RetrievePreparedImageResultRunning(token: Long) extends RetrievePreparedImageRestResult

class RetrievePreparedImageResultOk(
  val serverResp: Option[PrepareResult],
  val image: Image
) extends RetrievePreparedImageRestResult

case class CaptureResultRunning(token: Long) extends CaptureRestResult

sealed trait CaptureStatus

object CaptureStatus {
  case object Ok extends CaptureStatus
  case object NoGoogleOcrApiKeyRegistered extends CaptureStatus
}

class CaptureResultOk(
  val status: CaptureStatus,
  val serverResp: imm.Seq[CaptureResult]
) extends CaptureRestResult

object CaptureResultOk {
  def parse(json: JsValue): CaptureResultOk = {
    val status = (json \ "status").asOpt[String].getOrElse("ok")
    if (status == "ok") {
      val capturedFields = (json \ "capturedFields").as[Seq[JsValue]]
      new CaptureResultOk(
        status = CaptureStatus.Ok,
        capturedFields.map { e =>
          CaptureResult(
            (e \ "fieldName").as[String],
            (e \ "base64Image").as[String],
            (e \ "text").as[String],
            (e \ "rawText").as[String]
          )
        }.toList
      )
    } else if (status == "noGoogleOcrApiKeyRegistered") {
      new CaptureResultOk(
        status = CaptureStatus.NoGoogleOcrApiKeyRegistered,
        serverResp = imm.Seq()
      )
    } else throw new RuntimeException("Unknown status '" + status + "'")
  }
}

case class CaptureResult(
  fieldName: String, base64Image: String, text: String, rawText: String
)

case class Revision(value: Long) extends AnyVal

case class SaveConfigResult(
  revision: Revision
)

case class SaveConfigResultOk(
  serverResp: SaveConfigResult
) extends SaveConfigRestResult

object SaveConfigResultOk {
  def parse(json: JsValue): SaveConfigResponse = SaveConfigResponse(
    SaveConfigResult(
      Revision((json \ "revision").as[String].toLong)
    )
  )
}

case class OpenConfigResult(
  revision: Revision,
  config: String,
  comment: String,
  createAd: Instant
)

case class OpenConfigResultOk(
  serverResp: OpenConfigResult
) extends OpenConfigRestResult

object OpenConfigResultOk {
  def parse(json: JsValue): OpenConfigResultOk = {
    val rec = (json \ "record").as[JsValue]
    OpenConfigResultOk(
      OpenConfigResult(
        Revision((rec \ "revision").as[String].toLong),
        (rec \ "config").as[String],
        (rec \ "comment").as[String],
        Instant.ofEpochMilli((rec \ "createdAt").as[String].toLong)
      )
    )
  }
}

case class RemoveConfigResultOk(
  removeCount: Int
) extends RemoveConfigRestResult

case class FormConfig(
  configName: String,
  revision: Revision,
  comment: String,
  createdAt: Instant
)

case class ListConfigResult(
  configTable: imm.Seq[FormConfig]
)

case class ListConfigResultOk(
  serverResp: ListConfigResult
) extends ListConfigRestResult
