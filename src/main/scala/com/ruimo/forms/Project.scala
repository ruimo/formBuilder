package com.ruimo.forms

import play.api.libs.json._
import play.api.libs.json.Reads._
import play.api.libs.functional.syntax._
import com.ruimo.scoins.Percent

import scala.concurrent.Future
import java.nio.file.Path
import javafx.scene.Cursor
import javafx.scene.input.MouseEvent

import com.ruimo.forms.common.OcrSettings

import scalafx.scene.image.Image
import scalafx.geometry.{Point2D, Rectangle2D}
import com.ruimo.graphics.twodim.Area
import play.api.libs.json._

import scalafx.scene.canvas.{GraphicsContext => SfxGraphicsContext}
import scala.collection.{immutable => imm}

case class SaveConfigResponse(
  result: SaveConfigResult
)

case class OpenConfigResponse(
  result: OpenConfigResult
)

case class CaptureResponse(
  result: imm.Seq[CaptureResult]
)

sealed trait SkewCorrectionDirection
object SkewCorrectionDirection {
  implicit object skewCorrectionDirectionFormat extends Format[SkewCorrectionDirection] {
    override def reads(jv: JsValue): JsResult[SkewCorrectionDirection] = jv.as[String] match {
      case "horizontal" => JsSuccess(SkewCorrectionDirectionHorizontal)
      case "vertical" => JsSuccess(SkewCorrectionDirectionVertical)
      case s: String => JsError("'" + s + "' is invalid for skew correction direction.")
    }

    override def writes(d: SkewCorrectionDirection): JsValue = d match {
      case SkewCorrectionDirectionHorizontal => JsString("horizontal")
      case SkewCorrectionDirectionVertical => JsString("vertical")
    }
  }
}

case object SkewCorrectionDirectionHorizontal extends SkewCorrectionDirection
case object SkewCorrectionDirectionVertical extends SkewCorrectionDirection

sealed trait Field extends Widget[Field] {
  type R <: Field
  def drawArea(formSize: (Double, Double)): Rectangle2D
  def draw(formSize: (Double, Double), gc: SfxGraphicsContext, isSelected: Boolean): Unit
  def possibleMouseOperation(formSize: (Double, Double), x: Double, y: Double): MouseOperation
  def move(formSize: (Double, Double), from: Point2D, to: Point2D): R
  def northResize(formSize: (Double, Double), from: Point2D, to: Point2D): R
  def eastResize(formSize: (Double, Double), from: Point2D, to: Point2D): R
  def westResize(formSize: (Double, Double), from: Point2D, to: Point2D): R
  def southResize(formSize: (Double, Double), from: Point2D, to: Point2D): R
  def northEastResize(formSize: (Double, Double), from: Point2D, to: Point2D): R
  def northWestResize(formSize: (Double, Double), from: Point2D, to: Point2D): R
  def southEastResize(formSize: (Double, Double), from: Point2D, to: Point2D): R
  def southWestResize(formSize: (Double, Double), from: Point2D, to: Point2D): R
}

case class DotRemoval(
  enabled: Boolean,
  condition: DotRemovalCondition
)

case class CropRectangle(
  enabled: Boolean,
  condition: CropRectangleCondition
)

case class RemoveRuledLine(
  enabled: Boolean,
  condition: RemoveRuledLineCondition
)

case class SkewCorrection(
  enabled: Boolean,
  condition: SkewCorrectionCondition
)

trait SkewCorrectionCondition {
  def direction: SkewCorrectionDirection
  def lineCount: Int
  def maxAngleToDetect: Double
  def detectAreaFrom: Percent
  def detectAreaTo: Percent
}

object SkewCorrectionCondition {
  import projects.project0.SkewCorrectionConditionImpl.skewCorrectionConditionImplWrites
  import projects.project0.SkewCorrectionConditionImpl.skewCorrectionConditionImplReads

  implicit object skewCorrectionConditionFormat extends Format[SkewCorrectionCondition] {
    override def reads(jv: JsValue): JsResult[SkewCorrectionCondition] = {
      JsSuccess(
        jv.as[SkewCorrectionConditionImpl]
      )
    }

    override def writes(obj: SkewCorrectionCondition): JsValue = obj match {
      case o: SkewCorrectionConditionImpl => skewCorrectionConditionImplWrites.writes(o)
    }
  }
}

object SkewCorrection {
  import SkewCorrectionCondition.skewCorrectionConditionFormat

  implicit object skewCorrectionFormat extends Format[SkewCorrection] {
    override def reads(jv: JsValue): JsResult[SkewCorrection] = {
      JsSuccess(
        SkewCorrection(
          (jv \ "enabled").as[Boolean],
          (jv \ "condition").as[SkewCorrectionCondition]
        )
      )
    }

    override def writes(obj: SkewCorrection): JsValue = Json.obj(
      "enabled" -> JsBoolean(obj.enabled),
      "condition" -> Json.toJson(obj.condition)
    )
  }

  implicit object percentFormat extends Format[Percent] {
    override def reads(jv: JsValue): JsResult[Percent] = {
      JsSuccess(Percent(jv.as[Double]))
    }

    override def writes(obj: Percent): JsValue = JsNumber(obj.value)
  }
}

trait DotRemovalCondition {
  def maxDotSize: Int
  def blackLevel: BlackLevel
}

object DotRemovalCondition {
  import DotRemovalConditionImpl.dotRemovalConditionImplWrites
  import DotRemovalConditionImpl.dotRemovalConditionImplReads

  implicit object dotRemovalConditionFormat extends Format[DotRemovalCondition] {
    override def reads(jv: JsValue): JsResult[DotRemovalCondition] = {
      JsSuccess(
        jv.as[DotRemovalConditionImpl]
      )
    }

    override def writes(obj: DotRemovalCondition): JsValue = obj match {
      case o: DotRemovalConditionImpl => dotRemovalConditionImplWrites.writes(o)
    }
  }
}

trait CropRectangleCondition {
  def errorAllowance: Int
  def topMargin: Double
  def leftMargin: Double
  def rightMargin: Double
  def bottomMargin: Double
  def slantAllowance: Int
  def blackLevel: Int
}

object CropRectangleCondition {
  import CropRectangleConditionImpl.cropRectangleConditionImplWrites
  import CropRectangleConditionImpl.cropRectangleConditionImplReads

  implicit object cropRectangleConditionFormat extends Format[CropRectangleCondition] {
    override def reads(jv: JsValue): JsResult[CropRectangleCondition] = {
      JsSuccess(
        jv.as[CropRectangleConditionImpl]
      )
    }

    override def writes(obj: CropRectangleCondition): JsValue = obj match {
      case o: CropRectangleConditionImpl => cropRectangleConditionImplWrites.writes(o)
    }
  }
}

trait RemoveRuledLineCondition {
  def lineDeltaX: Int
  def lineDeltaY: Int
  def lineDotRatio: Int
  def correctOverlappingEnabled: Boolean
  def correctOverlappingDelta: Int
  def correctOverlappingDotRatio: Int
}

object RemoveRuledLineCondition {
  import RemoveRuledLineConditionImpl.removeRuledLineConditionImplWrites
  import RemoveRuledLineConditionImpl.removeRuledLineConditionImplReads

  implicit object removeRuledLineConditionFormat extends Format[RemoveRuledLineCondition] {
    override def reads(jv: JsValue): JsResult[RemoveRuledLineCondition] = {
      JsSuccess(
        jv.as[RemoveRuledLineConditionImpl]
      )
    }

    override def writes(obj: RemoveRuledLineCondition): JsValue = obj match {
      case o: RemoveRuledLineConditionImpl => removeRuledLineConditionImplWrites.writes(o)
    }
  }
}

object DotRemoval {
  import DotRemovalCondition.dotRemovalConditionFormat

  implicit object dotRemovalFormat extends Format[DotRemoval] {
    override def reads(jv: JsValue): JsResult[DotRemoval] = {
      JsSuccess(
        DotRemoval(
          (jv \ "enabled").as[Boolean],
          (jv \ "condition").as[DotRemovalCondition]
        )
      )
    }

    override def writes(obj: DotRemoval): JsValue = Json.obj(
      "enabled" -> JsBoolean(obj.enabled),
      "condition" -> Json.toJson(obj.condition)
    )
  }
}

object CropRectangle {
  import CropRectangleCondition.cropRectangleConditionFormat

  implicit object cropRectangleFormat extends Format[CropRectangle] {
    override def reads(jv: JsValue): JsResult[CropRectangle] = {
      JsSuccess(
        CropRectangle(
          (jv \ "enabled").as[Boolean],
          (jv \ "condition").as[CropRectangleCondition]
        )
      )
    }

    override def writes(obj: CropRectangle): JsValue = Json.obj(
      "enabled" -> JsBoolean(obj.enabled),
      "condition" -> Json.toJson(obj.condition)
    )
  }
}

object RemoveRuledLine {
  import RemoveRuledLineCondition.removeRuledLineConditionFormat

  implicit object removeRuleLineFormat extends Format[RemoveRuledLine] {
    override def reads(jv: JsValue): JsResult[RemoveRuledLine] = {
      JsSuccess(
        RemoveRuledLine(
          (jv \ "enabled").as[Boolean],
          (jv \ "condition").as[RemoveRuledLineCondition]
        )
      )
    }

    override def writes(obj: RemoveRuledLine): JsValue = Json.obj(
      "enabled" -> JsBoolean(obj.enabled),
      "condition" -> Json.toJson(obj.condition)
    )
  }
}

case class EdgeCrop(
  enabled: Boolean,
  condition: EdgeCropCondition
) {
  def asJson(en: Boolean = enabled): JsObject = {
    JsObject(
      Seq(
        "enabled" -> JsBoolean(en)
      ) ++ (
        if (en) condition.asJson.fields
        else Seq()
      )
    )
  }
}

// 0 - 254
final class EdgeCropSensivity(val value: Int) extends AnyVal {
  override def toString = "EdgeCropSensivity(" + value + ")"
}

object EdgeCropSensivity {
  def apply(value: Int): EdgeCropSensivity = {
    if (value < 0 || 254 < value)
      throw new IllegalArgumentException("black level(=" + value + ") should be 0-254")
    new EdgeCropSensivity(value)
  }
}

// 0 - 254
final class BlackLevel(val value: Int) extends AnyVal {
  override def toString = "BlackLevel(" + value + ")"
}

object BlackLevel {
  def apply(value: Int): BlackLevel = {
    if (value < 0 || 254 < value)
      throw new IllegalArgumentException("black level(=" + value + ") should be 0-254")
    new BlackLevel(value)
  }

  implicit object blackLevelFormat extends Format[BlackLevel] {
    override def reads(jv: JsValue): JsResult[BlackLevel] = {
      JsSuccess(
        BlackLevel(
          jv.as[Int]
        )
      )
    }

    override def writes(obj: BlackLevel): JsValue = Json.obj(
      "value" -> JsNumber(obj.value)
    )
  }
}

trait EdgeCropCondition {
  def topArea: Option[Area]
  def bottomArea: Option[Area]
  def leftArea: Option[Area]
  def rightArea: Option[Area]
  def topSensivity: EdgeCropSensivity
  def bottomSensivity: EdgeCropSensivity
  def leftSensivity: EdgeCropSensivity
  def rightSensivity: EdgeCropSensivity

  def asJson: JsObject
}

trait RectField extends Field {
  type R <: RectField
  def withNewRect(rect: Rectangle2D, formSize: (Double, Double)): R
  def rect: Rectangle2D
  def originalSize: (Double, Double)
  def scaleX(currentSize: (Double, Double), x: Double): Double =
    x * currentSize._1 / originalSize._1
  // Not bug! Use width (not height) to scale y axis too.
  def scaleY(currentSize: (Double, Double), y: Double): Double =
    y * currentSize._1 / originalSize._1

  def move(formSize: (Double, Double), from: Point2D, to: Point2D): R = {
    withNewRect(
      new Rectangle2D(
        scaleX(formSize, this.rect.minX) + (to.x - from.x),
        scaleY(formSize, this.rect.minY) + (to.y - from.y),
        scaleX(formSize, this.rect.width),
        scaleY(formSize, this.rect.height)
      ),
      formSize
    )
  }

  override def northResize(formSize: (Double, Double), from: Point2D, to: Point2D): R = {
    var diff = to.y - from.y
    val h = scaleY(formSize, this.rect.height)
    if (h <= diff) {
      diff = h - 1
    }

    withNewRect(
      new Rectangle2D(
        scaleX(formSize, this.rect.minX),
        scaleY(formSize, this.rect.minY) + diff,
        scaleX(formSize, this.rect.width),
        h - diff
      ),
      formSize
    )
  }

  override def southResize(formSize: (Double, Double), from: Point2D, to: Point2D): R = {
    var diff = from.y - to.y
    val h = scaleY(formSize, this.rect.height)
    if (h <= diff) {
      diff = h - 1
    }

    withNewRect(
      new Rectangle2D(
        scaleX(formSize, this.rect.minX),
        scaleY(formSize, this.rect.minY),
        scaleX(formSize, this.rect.width),
        h - diff
      ),
      formSize
    )
  }

  override def westResize(formSize: (Double, Double), from: Point2D, to: Point2D): R = {
    var diff = to.x - from.x
    val w = scaleX(formSize, this.rect.width)
    if (w <= diff) {
      diff = w - 1
    }

    withNewRect(
      new Rectangle2D(
        scaleX(formSize, this.rect.minX) + diff,
        scaleY(formSize, this.rect.minY),
        w - diff,
        scaleY(formSize, this.rect.height)
      ),
      formSize
    )
  }

  override def eastResize(formSize: (Double, Double), from: Point2D, to: Point2D): R = {
    var diff = from.x - to.x
    val w = scaleX(formSize, this.rect.width)
    if (w <= diff) {
      diff = w - 1
    }

    withNewRect(
      new Rectangle2D(
        scaleX(formSize, this.rect.minX),
        scaleY(formSize, this.rect.minY),
        w - diff,
        scaleY(formSize, this.rect.height)
      ),
      formSize
    )
  }

  override def northEastResize(formSize: (Double, Double), from: Point2D, to: Point2D): R = {
    var diffX = from.x - to.x
    var diffY = to.y - from.y
    val w = scaleX(formSize, this.rect.width)
    val h = scaleY(formSize, this.rect.height)
    if (w <= diffX) {
      diffX = w - 1
    }
    if (h <= diffY) {
      diffY = h - 1
    }

    withNewRect(
      new Rectangle2D(
        scaleX(formSize, this.rect.minX),
        scaleY(formSize, this.rect.minY) + diffY,
        w - diffX,
        h - diffY
      ),
      formSize
    )
  }

  override def northWestResize(formSize: (Double, Double), from: Point2D, to: Point2D): R = {
    var diffX = to.x - from.x
    var diffY = to.y - from.y
    val w = scaleX(formSize, this.rect.width)
    val h = scaleY(formSize, this.rect.height)
    if (w <= diffX) {
      diffX = w - 1
    }
    if (h <= diffY) {
      diffY = h - 1
    }

    withNewRect(
      new Rectangle2D(
        scaleX(formSize, this.rect.minX) + diffX,
        scaleY(formSize, this.rect.minY) + diffY,
        w - diffX,
        h - diffY
      ),
      formSize
    )
  }

  override def southWestResize(formSize: (Double, Double), from: Point2D, to: Point2D): R = {
    var diffX = to.x - from.x
    var diffY = from.y - to.y
    val w = scaleX(formSize, this.rect.width)
    val h = scaleY(formSize, this.rect.height)
    if (w <= diffX) {
      diffX = w - 1
    }
    if (h <= diffY) {
      diffY = h - 1
    }

    withNewRect(
      new Rectangle2D(
        scaleX(formSize, this.rect.minX) + diffX,
        scaleY(formSize, this.rect.minY),
        w - diffX,
        h - diffY
      ),
      formSize
    )
  }

  override def southEastResize(formSize: (Double, Double), from: Point2D, to: Point2D): R = {
    var diffX = from.x - to.x
    var diffY = from.y - to.y
    val w = scaleX(formSize, this.rect.width)
    val h = scaleY(formSize, this.rect.height)
    if (w <= diffX) {
      diffX = w - 1
    }
    if (h <= diffY) {
      diffY = h - 1
    }

    withNewRect(
      new Rectangle2D(
        scaleX(formSize, this.rect.minX),
        scaleY(formSize, this.rect.minY),
        w - diffX,
        h - diffY
      ),
      formSize
    )
  }
}

trait AbsoluteField extends Field with RectField {
  type R <: AbsoluteField
  def name: String
  def ocrSettings: Option[OcrSettings]
  def withNewValue(newName: String = name, newOcrSettings: Option[OcrSettings] = ocrSettings): R
}

trait CropField extends Field with RectField {
  type R <: CropField
  def toLeft: LeftCropField
  def toTop: TopCropField
  def toRight: RightCropField
  def toBottom: BottomCropField
  def asJson: JsObject
}

trait LeftCropField extends CropField {
  type R <: LeftCropField
}

trait TopCropField extends CropField {
  type R <: TopCropField
}

trait RightCropField extends CropField {
  type R <: RightCropField
}

trait BottomCropField extends CropField {
  type R <: BottomCropField
}

trait ProjectListener {
  def onSkewCorrectionChanged(skewCorrection: SkewCorrection)
  def onCropEnabledChanged(enabled: Boolean)
  def onDotRemovalChanged(dotRemoval: DotRemoval)
  def onCropRectangleChanged(cropRectangle: CropRectangle)
  def onRemoveRuledLineChanged(removeRuledLine: RemoveRuledLine)
}

object NullObjectListener extends ProjectListener {
  override def onSkewCorrectionChanged(skewCorrection: SkewCorrection) {}
  override def onCropEnabledChanged(enabled: Boolean) {}
  override def onDotRemovalChanged(dotRemoval: DotRemoval) {}
  override def onCropRectangleChanged(cropRectangle: CropRectangle) {}
  override def onRemoveRuledLineChanged(removeRuledLine: RemoveRuledLine) {}
}

case class CacheCondition(
  isSkewCorrectionEnabled: Boolean,
  isCropEnabled: Boolean,
  isDotRemovalEnabled: Boolean,
  isCropRectangleEnabled: Boolean,
  isRemoveRuledLineEnabled: Boolean
)

case class CacheConditionGlob(
  isSkewCorrectionEnabled: Option[Boolean] = None,
  isCropEnabled: Option[Boolean] = None,
  isDotRemovalEnabled: Option[Boolean] = None,
  isCropRectangleEnabled: Option[Boolean] = None,
  isRemoveRuledLineEnabled: Option[Boolean] = None
) {
  def isMatched(cond: CacheCondition): Boolean =
    isSkewCorrectionEnabled.map(_ == cond.isSkewCorrectionEnabled).getOrElse(true) &&
    isCropEnabled.map(_ == cond.isCropEnabled).getOrElse(true) &&
    isDotRemovalEnabled.map(_ == cond.isDotRemovalEnabled).getOrElse(true) &&
    isCropRectangleEnabled.map(_ == cond.isCropRectangleEnabled).getOrElse(true) &&
    isRemoveRuledLineEnabled.map(_ == cond.isRemoveRuledLineEnabled).getOrElse(true)
}

trait Project {
  def isDirty: Boolean
  def listener: ProjectListener

  def withListener(newListener: ProjectListener): Project
  def addAbsoluteField(f: AbsoluteField, isSelected: Boolean, redraw: Boolean = true): Unit
  def absoluteFields: AbsoluteFieldTable
  def deselectAllFields(): Unit
  def selectAbsoluteFields(formSize: (Double, Double), rect: Rectangle2D, e: MouseEvent): Unit
  def selectCropFields(formSize: (Double, Double), rect: Rectangle2D, e: MouseEvent): Unit
  def selectSingleFieldAt(formSize: (Double, Double), x: Double, y: Double): Unit
  def selectField(f: Field): Unit
  def selectAbsoluteField(f: AbsoluteField): Unit
  def selectCropField(f: CropField): Unit
  def deleteAllSelectedFields(): Unit
  def getSelectedFieldAt(formSize: (Double, Double), x: Double, y: Double): Option[Field]
  def getSelectedAbsoluteFieldAt(formSize: (Double, Double), x: Double, y: Double): Option[AbsoluteField]
  def getSelectedCropFieldAt(formSize: (Double, Double), x: Double, y: Double): Option[CropField]
  def getNormalFieldAt(formSize: (Double, Double), x: Double, y: Double): Option[Field]
  def getNormalAbsoluteFieldAt(formSize: (Double, Double), x: Double, y: Double): Option[AbsoluteField]
  def getNormalCropFieldAt(formSize: (Double, Double), x: Double, y: Double): Option[CropField]
  def moveSelectedFields(formSize: (Double, Double), from: Point2D, to: Point2D): Unit
  def moveSelectedAbsoluteFields(formSize: (Double, Double), from: Point2D, to: Point2D): Unit
  def moveSelectedCropFields(formSize: (Double, Double), from: Point2D, to: Point2D): Unit
  def renameSelectedAbsoluteField(f: AbsoluteField, newName: String): Unit
  def updateSelectedAbsoluteField(f: AbsoluteField, newName: String, newOcrSettings: Option[OcrSettings]): Unit
  def isAbsoluteFieldNameAlreadyUsed(name: String): Boolean
  def redraw(): Unit
  def possibleMouseOperation(formSize: (Double, Double), x: Double, y: Double): MouseOperation
  def northResizeSelectedFields(formSize: (Double, Double), p0: Point2D, p1: Point2D)
  def eastResizeSelectedFields(formSize: (Double, Double), p0: Point2D, p1: Point2D)
  def westResizeSelectedFields(formSize: (Double, Double), p0: Point2D, p1: Point2D)
  def southResizeSelectedFields(formSize: (Double, Double), p0: Point2D, p1: Point2D)
  def northWestResizeSelectedFields(formSize: (Double, Double), p0: Point2D, p1: Point2D)
  def northEastResizeSelectedFields(formSize: (Double, Double), p0: Point2D, p1: Point2D)
  def southWestResizeSelectedFields(formSize: (Double, Double), p0: Point2D, p1: Point2D)
  def southEastResizeSelectedFields(formSize: (Double, Double), p0: Point2D, p1: Point2D)

  def skewCorrection: SkewCorrection
  def skewCorrection_=(newSkewCorrection: SkewCorrection)

  def dotRemoval: DotRemoval
  def dotRemoval_=(newDotRemoval: DotRemoval)

  def cropRectangle: CropRectangle
  def cropRectangle_=(newCropRectangle: CropRectangle)

  def removeRuledLine: RemoveRuledLine
  def removeRuledLine_=(newRemoveRuledLine: RemoveRuledLine)

  def edgeCrop(
    formWidth: Double, formHeight: Double,
    topSensivity: EdgeCropSensivity, bottomSensivity: EdgeCropSensivity, leftSensivity: EdgeCropSensivity, rightSensivity: EdgeCropSensivity
  ): EdgeCrop
  def cropEnabled_=(enabled: Boolean)
  def cropEnabled: Boolean
  def topEdgeCropSensivity_=(topSensivity: EdgeCropSensivity)
  def topEdgeCropSensivity: EdgeCropSensivity
  def bottomEdgeCropSensivity_=(bottomSensivity: EdgeCropSensivity)
  def bottomEdgeCropSensivity: EdgeCropSensivity
  def leftEdgeCropSensivity_=(leftSensivity: EdgeCropSensivity)
  def leftEdgeCropSensivity: EdgeCropSensivity
  def rightEdgeCropSensivity_=(rightSensivity: EdgeCropSensivity)
  def rightEdgeCropSensivity: EdgeCropSensivity

  def addLeftCropField(f: LeftCropField, selected: Boolean, redraw: Boolean = true): Option[LeftCropField]
  def addRightCropField(f: RightCropField, selected: Boolean, redraw: Boolean = true): Option[RightCropField]
  def addTopCropField(f: TopCropField, selected: Boolean, redraw: Boolean = true): Option[TopCropField]
  def addBottomCropField(f: BottomCropField, selected: Boolean, redraw: Boolean = true): Option[BottomCropField]

  def leftCropField: Option[LeftCropField]
  def topCropField: Option[TopCropField]
  def rightCropField: Option[RightCropField]
  def bottomCropField: Option[BottomCropField]

  def isTopCropFieldSelected: Boolean
  def isLeftCropFieldSelected: Boolean
  def isRightCropFieldSelected: Boolean
  def isBottomCropFieldSelected: Boolean

  def selectLeftCropField(selected: Boolean): Unit
  def selectTopCropField(selected: Boolean): Unit
  def selectRightCropField(selected: Boolean): Unit
  def selectBottomCropField(selected: Boolean): Unit

  def cachedImage(
    file: SelectedImage,
    cacheCondition: CacheCondition = CacheCondition(
      isSkewCorrectionEnabled = skewCorrection.enabled,
      isCropEnabled = cropEnabled,
      isDotRemovalEnabled = dotRemoval.enabled,
      isCropRectangleEnabled = cropRectangle.enabled,
      isRemoveRuledLineEnabled = removeRuledLine.enabled
    )
  ): Either[RetrievePreparedImageRestResult, Future[RetrievePreparedImageRestResult]]

  def invalidateCachedImage(
    cacheCondition: CacheConditionGlob = CacheConditionGlob()
  ): Unit
  def runCapture(si: SelectedImage): Future[Either[CaptureRestFailure, CaptureResultOk]]
  def cropFieldsAreReady: Boolean
  def listConfig(): Future[ListConfigRestResult]
  def saveConfig(configName: String, imageSize: (Double, Double)): Future[SaveConfigRestResult]
  def removeConfig(configName: String): Future[RemoveConfigRestResult]
  def openConfig(configName: String): Future[OpenConfigRestResult]
}

class ProjectContext(
  val onNormalAbsoluteFieldAdded: AbsoluteField => Unit,
  val onSelectedAbsoluteFieldAdded: AbsoluteField => Unit,
  val onNormalAbsoluteFieldRemoved: AbsoluteField => Unit,
  val onSelectedAbsoluteFieldRemoved: AbsoluteField => Unit,
  val onNormalCropFieldAdded: CropField => Unit,
  val onSelectedCropFieldAdded: CropField => Unit,
  val onNormalCropFieldRemoved: CropField => Unit,
  val onSelectedCropFieldRemoved: CropField => Unit,
  val redrawCropField: CropField => Unit,
  val selectCropField: (CropField, Boolean) => Unit
)
