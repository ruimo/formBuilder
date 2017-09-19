package com.ruimo.forms

import javafx.scene.Cursor
import scalafx.geometry.{Point2D, Rectangle2D}
import javafx.scene.input.MouseEvent

import com.ruimo.graphics.twodim.Area
import play.api.libs.json.{JsObject, JsString, JsValue}

import scalafx.scene.canvas.{GraphicsContext => SfxGraphicsContext}
import scala.collection.{immutable => imm}

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
  def drawArea: Rectangle2D
  def draw(gc: SfxGraphicsContext, isSelected: Boolean): Unit
  def possibleMouseOperation(x: Double, y: Double): MouseOperation
  def move(from: Point2D, to: Point2D): R
  def northResize(from: Point2D, to: Point2D): R
  def eastResize(from: Point2D, to: Point2D): R
  def westResize(from: Point2D, to: Point2D): R
  def southResize(from: Point2D, to: Point2D): R
  def northEastResize(from: Point2D, to: Point2D): R
  def northWestResize(from: Point2D, to: Point2D): R
  def southEastResize(from: Point2D, to: Point2D): R
  def southWestResize(from: Point2D, to: Point2D): R
}

trait SkewCorrection {
  def enabled: Boolean
  def direction: SkewCorrectionDirection
  def lineCount: Int
  def maxAngleToDetect: Double

  def withEnabled(newEnabled: Boolean): SkewCorrection
  def asJson: JsValue
}

trait EdgeCrop {
  def enabled: Boolean
  def topArea: Option[Area]
  def bottomArea: Option[Area]
  def leftArea: Option[Area]
  def rightArea: Option[Area]

  def withEnabled(newEnabled: Boolean): EdgeCrop
  def asJson: JsValue
}

trait RectField extends Field {
  type R <: RectField
  def withNewRect(rect: Rectangle2D): R
  def rect: Rectangle2D

  def move(from: Point2D, to: Point2D): R = withNewRect(
    new Rectangle2D(
      this.rect.minX + (to.x - from.x),
      this.rect.minY + (to.y - from.y),
      this.rect.width,
      this.rect.height
    )
  )

  override def northResize(from: Point2D, to: Point2D): R = {
    var diff = to.y - from.y
    if (this.rect.height <= diff) {
      diff = this.rect.height - 1
    }

    withNewRect(
      new Rectangle2D(
        this.rect.minX, this.rect.minY + diff,
        this.rect.width, this.rect.height - diff
      )
    )
  }

  override def southResize(from: Point2D, to: Point2D): R = {
    var diff = from.y - to.y
    if (this.rect.height <= diff) {
      diff = this.rect.height - 1
    }

    withNewRect(
      new Rectangle2D(
        this.rect.minX, this.rect.minY,
        this.rect.width, this.rect.height - diff
      )
    )
  }

  override def westResize(from: Point2D, to: Point2D): R = {
    var diff = to.x - from.x
    if (this.rect.width <= diff) {
      diff = this.rect.width - 1
    }

    withNewRect(
      new Rectangle2D(
        this.rect.minX + diff, this.rect.minY,
        this.rect.width - diff, this.rect.height
      )
    )
  }

  override def eastResize(from: Point2D, to: Point2D): R = {
    var diff = from.x - to.x
    if (this.rect.width <= diff) {
      diff = this.rect.width - 1
    }

    withNewRect(
      new Rectangle2D(
        this.rect.minX, this.rect.minY,
        this.rect.width - diff, this.rect.height
      )
    )
  }

  override def northEastResize(from: Point2D, to: Point2D): R = {
    var diffX = from.x - to.x
    var diffY = to.y - from.y
    if (this.rect.width <= diffX) {
      diffX = this.rect.width - 1
    }
    if (this.rect.height <= diffY) {
      diffY = this.rect.height - 1
    }

    withNewRect(
      new Rectangle2D(
        this.rect.minX, this.rect.minY + diffY,
        this.rect.width - diffX, this.rect.height - diffY
      )
    )
  }

  override def northWestResize(from: Point2D, to: Point2D): R = {
    var diffX = to.x - from.x
    var diffY = to.y - from.y
    if (this.rect.width <= diffX) {
      diffX = this.rect.width - 1
    }
    if (this.rect.height <= diffY) {
      diffY = this.rect.height - 1
    }

    withNewRect(
      new Rectangle2D(
        this.rect.minX + diffX, this.rect.minY + diffY,
        this.rect.width - diffX, this.rect.height - diffY
      )
    )
  }

  override def southWestResize(from: Point2D, to: Point2D): R = {
    var diffX = to.x - from.x
    var diffY = from.y - to.y
    if (this.rect.width <= diffX) {
      diffX = this.rect.width - 1
    }
    if (this.rect.height <= diffY) {
      diffY = this.rect.height - 1
    }

    withNewRect(
      new Rectangle2D(
        this.rect.minX + diffX, this.rect.minY,
        this.rect.width - diffX, this.rect.height - diffY
      )
    )
  }

  override def southEastResize(from: Point2D, to: Point2D): R = {
    var diffX = from.x - to.x
    var diffY = from.y - to.y
    if (this.rect.width <= diffX) {
      diffX = this.rect.width - 1
    }
    if (this.rect.height <= diffY) {
      diffY = this.rect.height - 1
    }

    withNewRect(
      new Rectangle2D(
        this.rect.minX, this.rect.minY,
        this.rect.width - diffX, this.rect.height - diffY
      )
    )
  }
}

trait AbsoluteField extends Field with RectField {
  type R <: AbsoluteField
  def name: String
  def withName(newName: String): R
}

trait CropField extends Field with RectField {
  type R <: CropField
  def toLeft: LeftCropField
  def toTop: TopCropField
  def toRight: RightCropField
  def toBottom: BottomCropField
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

class AbsoluteFieldTable(
  projectContext: ProjectContext
) {
  private[this] var _normalAbsoluteFields: imm.Seq[AbsoluteField] = imm.Seq()
  private[this] var _selectedAbsoluteFields: imm.Seq[AbsoluteField] = imm.Seq()

  def normalFields: imm.Seq[AbsoluteField] = _normalAbsoluteFields
  def selectedFields: imm.Seq[AbsoluteField] = _selectedAbsoluteFields

  def addAbsoluteField(f: AbsoluteField, isSelected: Boolean) {
    if (isSelected)
      _selectedAbsoluteFields= _selectedAbsoluteFields :+ f
    else
      _normalAbsoluteFields = _normalAbsoluteFields :+ f

    (if (isSelected) projectContext.onSelectedAbsoluteFieldAdded else projectContext.onNormalAbsoluteFieldAdded).apply(f)
  }

  def deselectAllFields() {
    selectedFields.foreach { af =>
      _normalAbsoluteFields  = _normalAbsoluteFields :+ af
      projectContext.onSelectedAbsoluteFieldRemoved(af)
      projectContext.onNormalAbsoluteFieldAdded(af)
    }

    _selectedAbsoluteFields = imm.Seq()
  }

  def selectFields(rect: Rectangle2D, e: MouseEvent) {
    _normalAbsoluteFields = _normalAbsoluteFields.filter { f =>
      if (f.intersects(rect)) {
        _selectedAbsoluteFields = _selectedAbsoluteFields :+ f
        projectContext.onSelectedAbsoluteFieldAdded(f)
        projectContext.onNormalAbsoluteFieldRemoved(f)
        false
      } else true
    }
  }

  def selectSingleAbsoluteFieldAt(x: Double, y: Double): Option[Field] = {
    val found = _normalAbsoluteFields.zipWithIndex.find { case (f, idx) =>
      f.contains(x, y)
    }

    found.foreach { case (f, idx) =>
        val sp = _normalAbsoluteFields.splitAt(idx)
        _normalAbsoluteFields = sp._1 ++ sp._2.tail
        _selectedAbsoluteFields = _selectedAbsoluteFields :+ f
        projectContext.onSelectedAbsoluteFieldAdded(f)
        projectContext.onNormalAbsoluteFieldRemoved(f)
    }

    found.map(_._1)
  }

  def selectAbsoluteField(af: AbsoluteField) {
    val (f, idx) = _normalAbsoluteFields.zipWithIndex.find { _._1 == af }.get

    val sp = _normalAbsoluteFields.splitAt(idx)
    _normalAbsoluteFields = sp._1 ++ sp._2.tail
    _selectedAbsoluteFields = _selectedAbsoluteFields :+ f
    projectContext.onSelectedAbsoluteFieldAdded(f)
    projectContext.onNormalAbsoluteFieldRemoved(f)
  }

  def deleteAllSelectedFields() {
    val buf = _selectedAbsoluteFields
    _selectedAbsoluteFields = imm.Seq()
    buf.foreach { f =>
      projectContext.onSelectedAbsoluteFieldRemoved(f)
    }
  }

  def getSelectedFieldAt(x: Double, y: Double): Option[AbsoluteField] =
    _selectedAbsoluteFields.find { f =>
      f.contains(x, y)
    }

  def getNormalFieldAt(x: Double, y: Double): Option[AbsoluteField] =
    _normalAbsoluteFields.find { f =>
      f.contains(x, y)
    }

  def moveSelectedFields(from: Point2D, to: Point2D) {
    modifySelectedFields(_.move(from, to))
  }

  def northResizeSelectedFields(from: Point2D, to: Point2D): Unit = {
    modifySelectedFields(_.northResize(from, to))
  }

  def eastResizeSelectedFields(from: Point2D, to: Point2D): Unit = {
    modifySelectedFields(_.eastResize(from, to))
  }

  def westResizeSelectedFields(from: Point2D, to: Point2D): Unit = {
    modifySelectedFields(_.westResize(from, to))
  }

  def southResizeSelectedFields(from: Point2D, to: Point2D) {
    modifySelectedFields(_.southResize(from, to))
  }

  def northWestResizeSelectedFields(from: Point2D, to: Point2D) {
    modifySelectedFields(_.northWestResize(from, to))
  }

  def northEastResizeSelectedFields(from: Point2D, to: Point2D) {
    modifySelectedFields(_.northEastResize(from, to))
  }

  def southWestResizeSelectedFields(from: Point2D, to: Point2D) {
    modifySelectedFields(_.southWestResize(from, to))
  }

  def southEastResizeSelectedFields(from: Point2D, to: Point2D) {
    modifySelectedFields(_.southEastResize(from, to))
  }

  def modifySelectedFields(modifier: AbsoluteField => AbsoluteField) {
    val orgFields = _selectedAbsoluteFields

    _selectedAbsoluteFields = _selectedAbsoluteFields.map { f =>
      modifier(f)
    }

    orgFields.foreach { f =>
      projectContext.onSelectedAbsoluteFieldRemoved(f)
    }

    _selectedAbsoluteFields.foreach { f =>
      projectContext.onSelectedAbsoluteFieldAdded(f)
    }
  }

  def renameSelectedField(af: AbsoluteField, newName: String) {
    if (af.name != newName) {
      val newField = af.withName(newName)
      _selectedAbsoluteFields.zipWithIndex.find { _._1 == af } match {
        case Some((f, idx)) =>
          val sp = _selectedAbsoluteFields.splitAt(idx)
          _selectedAbsoluteFields = (sp._1 :+ f.withName(newName)) ++ sp._2.tail
        case None =>
      }
    }
  }

  def redraw() {
    _normalAbsoluteFields.foreach { f =>
      projectContext.onNormalAbsoluteFieldRemoved(f)
      projectContext.onNormalAbsoluteFieldAdded(f)
    }

    _selectedAbsoluteFields.foreach { f =>
      projectContext.onSelectedAbsoluteFieldRemoved(f)
      projectContext.onSelectedAbsoluteFieldAdded(f)
    }
  }

  def possibleMouseOperation(x: Double, y: Double): MouseOperation = {
    _selectedAbsoluteFields.find { f =>
      f.drawArea.contains(x, y)
    }.orElse {
      _normalAbsoluteFields.find { f =>
        f.drawArea.contains(x, y)
      }
    } match {
      case None => CanDoNothing
      case Some(f) => f.possibleMouseOperation(x, y)
    }
  }
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
  val version: Version
  def listener: ProjectListener

  def withListener(newListener: ProjectListener): Project
  def addAbsoluteField(f: AbsoluteField, isSelected: Boolean): Unit
  def absoluteFields: AbsoluteFieldTable
  def deselectAllFields(): Unit
  def selectAbsoluteFields(rect: Rectangle2D, e: MouseEvent): Unit
  def selectCropFields(rect: Rectangle2D, e: MouseEvent): Unit
  def selectSingleFieldAt(x: Double, y: Double): Unit
  def selectField(f: Field): Unit
  def selectAbsoluteField(f: AbsoluteField): Unit
  def selectCropField(f: CropField): Unit
  def deleteAllSelectedFields(): Unit
  def getSelectedFieldAt(x: Double, y: Double): Option[Field]
  def getSelectedAbsoluteFieldAt(x: Double, y: Double): Option[AbsoluteField]
  def getSelectedCropFieldAt(x: Double, y: Double): Option[CropField]
  def getNormalFieldAt(x: Double, y: Double): Option[Field]
  def getNormalAbsoluteFieldAt(x: Double, y: Double): Option[AbsoluteField]
  def getNormalCropFieldAt(x: Double, y: Double): Option[CropField]
  def moveSelectedFields(from: Point2D, to: Point2D): Unit
  def moveSelectedAbsoluteFields(from: Point2D, to: Point2D): Unit
  def moveSelectedCropFields(from: Point2D, to: Point2D): Unit
  def renameSelectedAbsoluteField(f: AbsoluteField, newName: String): Unit
  def redraw(): Unit
  def possibleMouseOperation(x: Double, y: Double): MouseOperation
  def northResizeSelectedFields(p0: Point2D, p1: Point2D)
  def eastResizeSelectedFields(p0: Point2D, p1: Point2D)
  def westResizeSelectedFields(p0: Point2D, p1: Point2D)
  def southResizeSelectedFields(p0: Point2D, p1: Point2D)
  def northWestResizeSelectedFields(p0: Point2D, p1: Point2D)
  def northEastResizeSelectedFields(p0: Point2D, p1: Point2D)
  def southWestResizeSelectedFields(p0: Point2D, p1: Point2D)
  def southEastResizeSelectedFields(p0: Point2D, p1: Point2D)

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
}

class ProjectContext(
  val onNormalAbsoluteFieldAdded: AbsoluteField => Unit,
  val onSelectedAbsoluteFieldAdded: AbsoluteField => Unit,
  val onNormalAbsoluteFieldRemoved: AbsoluteField => Unit,
  val onSelectedAbsoluteFieldRemoved: AbsoluteField => Unit,
  val onNormalCropFieldAdded: CropField => Unit,
  val onSelectedCropFieldAdded: CropField => Unit,
  val onNormalCropFieldRemoved: CropField => Unit,
  val onSelectedCropFieldRemoved: CropField => Unit
)
