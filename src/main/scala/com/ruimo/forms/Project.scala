package com.ruimo.forms

import com.ruimo.scoins.Percent
import scala.concurrent.Future
import java.nio.file.Path
import javafx.scene.Cursor
import javafx.scene.input.MouseEvent

import scalafx.scene.image.Image
import scalafx.geometry.{Point2D, Rectangle2D}
import com.ruimo.graphics.twodim.Area
import play.api.libs.json.{JsBoolean, JsObject, JsString, JsValue}

import scalafx.scene.canvas.{GraphicsContext => SfxGraphicsContext}
import scala.collection.{immutable => imm}

case class SaveConfigResponse(
  result: SaveConfigResult
)

case class CaptureResponse(
  result: imm.Seq[CaptureResult]
)

sealed trait SkewCorrectionDirection {
  def asJson: JsValue
}
case object SkewCorrectionDirectionHorizontal extends SkewCorrectionDirection {
  val asJson = JsString("horizontal")
}
case object SkewCorrectionDirectionVertical extends SkewCorrectionDirection {
  val asJson = JsString("vertical")
}

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

case class SkewCorrection(
  enabled: Boolean,
  condition: SkewCorrectionCondition
) {
  def asJson(en: Boolean = enabled): JsObject = JsObject(
    Seq(
      "enabled" -> JsBoolean(en)
    ) ++ condition.asJson.fields
  )
}

trait SkewCorrectionCondition {
  def direction: SkewCorrectionDirection
  def lineCount: Int
  def maxAngleToDetect: Double

  def asJson: JsObject
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
        if (en) condition.asJson.fields else Seq()
      )
    )
  }
}

trait EdgeCropCondition {
  def topArea: Option[Area]
  def bottomArea: Option[Area]
  def leftArea: Option[Area]
  def rightArea: Option[Area]

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
  def withName(newName: String): R
  def asJson: JsObject
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
}

object NullObjectListener extends ProjectListener {
  def onSkewCorrectionChanged(skewCorrection: SkewCorrection) {}
  def onCropEnabledChanged(enabled: Boolean) {}
}

trait Project {
  def isDirty: Boolean
  val version: Version
  def listener: ProjectListener

  def withListener(newListener: ProjectListener): Project
  def addAbsoluteField(f: AbsoluteField, isSelected: Boolean): Unit
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

  def edgeCrop(formWidth: Double, formHeight: Double): EdgeCrop
  def cropEnabled_=(enabled: Boolean)
  def cropEnabled: Boolean

  def addLeftCropField(f: LeftCropField, selected: Boolean): Option[LeftCropField]
  def addRightCropField(f: RightCropField, selected: Boolean): Option[RightCropField]
  def addTopCropField(f: TopCropField, selected: Boolean): Option[TopCropField]
  def addBottomCropField(f: BottomCropField, selected: Boolean): Option[BottomCropField]

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
    isSkewCorrectionEnabled: Boolean = skewCorrection.enabled,
    isCropEnabled: Boolean = cropEnabled
  ): Either[RetrievePreparedImageRestResult, Future[RetrievePreparedImageRestResult]]

  def invalidateCachedImage(skewCorrected: Boolean = false, cropped: Boolean = false): Unit
  def runCapture(si: SelectedImage): Future[Either[CaptureRestFailure, CaptureResultOk]]
  def cropFieldsAreReady: Boolean
  def listConfig(): Future[ListConfigRestResult]
  def saveConfig(configName: String): Future[SaveConfigRestResult]
  def removeConfig(configName: String): Future[RemoveConfigRestResult]
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
