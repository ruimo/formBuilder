package com.ruimo.forms.projects.project0

import scala.concurrent.ExecutionContext.Implicits.global
import java.io.FileInputStream
import java.net.{HttpURLConnection, URL}
import java.nio.file.{Files, Path}
import java.util.zip.ZipInputStream
import javafx.scene.input.MouseEvent
import javafx.scene.paint.Color

import com.ruimo.forms._
import play.api.libs.json._

import scala.collection.{immutable => imm}
import scalafx.geometry.{Point2D, Rectangle2D}
import scalafx.scene.canvas.{GraphicsContext => SfxGraphicsContext}
import com.ruimo.graphics.twodim.Area
import com.ruimo.scoins.{Percent, Zip}

import scalafx.scene.image.Image
import com.ruimo.scoins.LoanPattern._

import scala.annotation.tailrec
import scala.concurrent.Future
import scala.util.{Failure, Success}

object AbsoluteFieldImpl {
  val LineWidth = 2.0
}

case class AbsoluteFieldImpl(rect: Rectangle2D, name: String) extends AbsoluteField {
  import AbsoluteFieldImpl._
  override type R = AbsoluteFieldImpl

  val drawArea = new Rectangle2D(
    rect.minX - LineWidth / 2, rect.minY - LineWidth / 2,
    rect.width + LineWidth, rect.height + LineWidth
  )

  def draw(gc: SfxGraphicsContext, isSelected: Boolean) {
    gc.setLineWidth(LineWidth)
    gc.setStroke(if (isSelected) Color.RED else Color.BLUE)
    gc.setLineDashes()
    gc.strokeRect(rect.minX, rect.minY, rect.width, rect.height)
  }

  def withNewRect(newRect: Rectangle2D): R = copy(rect = newRect)

  def withName(newName: String): AbsoluteFieldImpl =
    if (newName != name) copy(name = newName) else this

  def possibleMouseOperation(x: Double, y: Double): MouseOperation = {
    val cornerSize = LineWidth * 2

    if (rect.minX - cornerSize <= x && x <= rect.minX + cornerSize
      && rect.minY - cornerSize <= y && y <= rect.minY + cornerSize) {
      CanNorthWestResize(this)
    }
    else if (rect.maxX - cornerSize <= x && x <= rect.maxX + cornerSize
      && rect.minY - cornerSize <= y && y <= rect.minY + cornerSize) {
      CanNorthEastResize(this)
    }
    else if (rect.minX - cornerSize <= x && x <= rect.minX + cornerSize
      && rect.maxY - cornerSize <= y && y <= rect.maxY + cornerSize) {
      CanSouthWestResize(this)
    }
    else if (rect.maxX - cornerSize <= x && x <= rect.maxX + cornerSize
      && rect.maxY - cornerSize <= y && y <= rect.maxY + cornerSize) {
      CanSouthEastResize(this)
    }
    else if (rect.minX - cornerSize <= x && x <= rect.minX + cornerSize) {
      CanWestResize(this)
    }
    else if (rect.maxX - cornerSize <= x && x <= rect.maxX + cornerSize) {
      CanEastResize(this)
    }
    else if (rect.minY - cornerSize <= y && y <= rect.minY + cornerSize) {
      CanNorthResize(this)
    }
    else if (rect.maxY - cornerSize <= y && y <= rect.maxY + cornerSize) {
      CanSouthResize(this)
    }
    else if (rect.contains(x, y)) {
      CanMove(this)
    }
    else CanDoNothing
  }
}

object CropFieldImpl {
  val LineWidth = 2.0
}

abstract class CropFieldImpl(rect: Rectangle2D) extends CropField {
  type R <: CropFieldImpl
  import CropFieldImpl._

  override def draw(gc: SfxGraphicsContext, isSelected: Boolean) {
    gc.setLineWidth(LineWidth)
    gc.setStroke(if (isSelected) Color.RED else Color.BLUE)
    gc.setLineDashes()
    gc.strokeRect(rect.minX, rect.minY, rect.width, rect.height)
  }

  override def drawArea: Rectangle2D = new Rectangle2D(
    rect.minX - LineWidth / 2, rect.minY - LineWidth / 2,
    rect.width + LineWidth, rect.height + LineWidth
  )

  override def possibleMouseOperation(x: Double, y: Double): MouseOperation = {
    val cornerSize = LineWidth * 2

    if (rect.minX - cornerSize <= x && x <= rect.minX + cornerSize
      && rect.minY - cornerSize <= y && y <= rect.minY + cornerSize) {
      CanNorthWestResize(this)
    }
    else if (rect.maxX - cornerSize <= x && x <= rect.maxX + cornerSize
      && rect.minY - cornerSize <= y && y <= rect.minY + cornerSize) {
      CanNorthEastResize(this)
    }
    else if (rect.minX - cornerSize <= x && x <= rect.minX + cornerSize
      && rect.maxY - cornerSize <= y && y <= rect.maxY + cornerSize) {
      CanSouthWestResize(this)
    }
    else if (rect.maxX - cornerSize <= x && x <= rect.maxX + cornerSize
      && rect.maxY - cornerSize <= y && y <= rect.maxY + cornerSize) {
      CanSouthEastResize(this)
    }
    else if (rect.minX - cornerSize <= x && x <= rect.minX + cornerSize) {
      CanWestResize(this)
    }
    else if (rect.maxX - cornerSize <= x && x <= rect.maxX + cornerSize) {
      CanEastResize(this)
    }
    else if (rect.minY - cornerSize <= y && y <= rect.minY + cornerSize) {
      CanNorthResize(this)
    }
    else if (rect.maxY - cornerSize <= y && y <= rect.maxY + cornerSize) {
      CanSouthResize(this)
    }
    else if (rect.contains(x, y)) {
      CanMove(this)
    }
    else CanDoNothing
  }

  override def toLeft: LeftCropField = LeftCropFieldImpl(rect)
  override def toRight: RightCropField = RightCropFieldImpl(rect)
  override def toBottom: BottomCropField = BottomCropFieldImpl(rect)
  override def toTop: TopCropField = TopCropFieldImpl(rect)
}

case class UnknownCropFieldImpl(rect: Rectangle2D) extends CropFieldImpl(rect) {
  override type R = UnknownCropFieldImpl
  def withNewRect(newRect: Rectangle2D): R = copy(rect = newRect)
}

case class TopCropFieldImpl(rect: Rectangle2D) extends CropFieldImpl(rect) with TopCropField {
  override type R = TopCropFieldImpl
  override def toTop: TopCropField = this
  override def draw(gc: SfxGraphicsContext, isSelected: Boolean) {
    import CropFieldImpl.LineWidth

    super.draw(gc, isSelected)
    if (rect.height > LineWidth && rect.width > LineWidth) {
      gc.strokeLine(
        rect.minX + LineWidth / 2, rect.minY + LineWidth / 2,
        rect.minX + rect.width / 2 - LineWidth / 2, rect.maxY - LineWidth / 2
      )
      gc.strokeLine(
        rect.minX + rect.width / 2 - LineWidth / 2, rect.maxY - LineWidth / 2,
        rect.maxX - LineWidth / 2, rect.minY + LineWidth / 2
      )
    }
  }
  def withNewRect(newRect: Rectangle2D): R = copy(rect = newRect)
}

case class LeftCropFieldImpl(rect: Rectangle2D) extends CropFieldImpl(rect) with LeftCropField {
  type R = LeftCropFieldImpl
  override def toLeft: LeftCropField = this
  override def draw(gc: SfxGraphicsContext, isSelected: Boolean) {
    import CropFieldImpl.LineWidth

    super.draw(gc, isSelected)
    if (rect.height > LineWidth && rect.width > LineWidth) {
      gc.strokeLine(
        rect.minX + LineWidth / 2, rect.minY + LineWidth / 2,
        rect.maxX - LineWidth / 2, rect.minY + rect.height / 2 - LineWidth / 2
      )
      gc.strokeLine(
        rect.maxX - LineWidth / 2, rect.minY + rect.height / 2 - LineWidth / 2,
        rect.minX + LineWidth / 2, rect.maxY - LineWidth / 2
      )
    }
  }
  def withNewRect(newRect: Rectangle2D): R = copy(rect = newRect)
}

case class RightCropFieldImpl(rect: Rectangle2D) extends CropFieldImpl(rect) with RightCropField {
  type R = RightCropFieldImpl
  override def toRight: RightCropField = this
  override def draw(gc: SfxGraphicsContext, isSelected: Boolean) {
    import CropFieldImpl.LineWidth

    super.draw(gc, isSelected)
    if (rect.height > LineWidth && rect.width > LineWidth) {
      gc.strokeLine(
        rect.maxX - LineWidth / 2, rect.minY + LineWidth / 2,
        rect.minX + LineWidth / 2, rect.minY + rect.height / 2 - LineWidth / 2
      )
      gc.strokeLine(
        rect.minX + LineWidth / 2, rect.minY + rect.height / 2 - LineWidth / 2,
        rect.maxX - LineWidth / 2, rect.maxY - LineWidth / 2
      )
    }
  }
  def withNewRect(newRect: Rectangle2D): R = copy(rect = newRect)
}

case class BottomCropFieldImpl(rect: Rectangle2D) extends CropFieldImpl(rect) with BottomCropField {
  type R = BottomCropFieldImpl
  override def toBottom: BottomCropField = this
  override def draw(gc: SfxGraphicsContext, isSelected: Boolean) {
    import CropFieldImpl.LineWidth

    super.draw(gc, isSelected)
    if (rect.height > LineWidth && rect.width > LineWidth) {
      gc.strokeLine(
        rect.minX + LineWidth / 2, rect.maxY - LineWidth / 2,
        rect.minX - LineWidth / 2 + rect.width / 2, rect.minY + LineWidth / 2
      )
      gc.strokeLine(
        rect.minX - LineWidth / 2 + rect.width / 2, rect.minY + LineWidth / 2,
        rect.maxX - LineWidth / 2, rect.maxY - LineWidth / 2
      )
    }
  }
  def withNewRect(newRect: Rectangle2D): this.type = copy(rect = newRect).asInstanceOf[this.type]
}

case class SkewCorrectionConditionImpl(
  direction: SkewCorrectionDirection = SkewCorrectionDirectionHorizontal,
  lineCount: Int = 1,
  maxAngleToDetect: Double = 2.0
) extends SkewCorrectionCondition {
  lazy val asJson: JsObject = Json.obj(
    "direction" -> direction.asJson,
    "lineCount" -> JsNumber(lineCount),
    "maxAngleToDetect" -> JsNumber(maxAngleToDetect)
  )
}

case class EdgeCropConditionImpl(
  topArea: Option[Area],
  bottomArea: Option[Area],
  leftArea: Option[Area],
  rightArea: Option[Area]
) extends EdgeCropCondition {
  lazy val asJson: JsObject = {
    def areaToJson(area: Area) = JsArray(
      Seq(
        JsNumber(area.x.value), JsNumber(area.y.value), JsNumber(area.w.value), JsNumber(area.h.value)
      )
    )

    JsObject(
      topArea.map { a => "top" -> areaToJson(a) }.toSeq ++
      bottomArea.map { a => "bottom" -> areaToJson(a) }.toSeq ++
      leftArea.map { a => "left" -> areaToJson(a) }.toSeq ++
      rightArea.map { a => "right" -> areaToJson(a) }.toSeq
    )
  }
}

object ProjectImpl {
  def apply(
    projectContext: ProjectContext,
    listener: ProjectListener = NullObjectListener
  ): ProjectImpl = new ProjectImpl(projectContext, listener)
}

class ProjectImpl(
  projectContext: ProjectContext,
  val listener: ProjectListener
) extends com.ruimo.forms.Project {
  val version = VersionOne
  @volatile
  private[this] var _skewCorrection: SkewCorrection = SkewCorrection(false, SkewCorrectionConditionImpl())
  @volatile
  private[this] var _absFields = new AbsoluteFieldTable(projectContext)
  @volatile
  private[this] var _leftCropField: Option[LeftCropField] = None
  @volatile
  private[this] var _rightCropField: Option[RightCropField] = None
  @volatile
  private[this] var _topCropField: Option[TopCropField] = None
  @volatile
  private[this] var _bottomCropField: Option[BottomCropField] = None
  @volatile
  private[this] var _isLeftCropFieldSelected: Boolean = false
  @volatile
  private[this] var _isRightCropFieldSelected: Boolean = false
  @volatile
  private[this] var _isTopCropFieldSelected: Boolean = false
  @volatile
  private[this] var _isBottomCropFieldSelected: Boolean = false
  @volatile
  private[this] var _isEdgeCropEnabled: Boolean = false
  @volatile
  private[this] var _cachedImage: imm.Map[(Path, Boolean, Boolean), (Option[PrepareResult], Image)] = Map()

  def withListener(newListener: ProjectListener): Project = new ProjectImpl(
    this.projectContext,
    newListener
  )

  def absoluteFields: AbsoluteFieldTable = _absFields

  def skewCorrection: SkewCorrection = _skewCorrection

  def skewCorrection_=(newSkewCorrection: SkewCorrection) {
    if (newSkewCorrection != _skewCorrection) {
      this._skewCorrection = newSkewCorrection
      listener.onSkewCorrectionChanged(newSkewCorrection)
    }
  }
  
  def addAbsoluteField(f: AbsoluteField, isSelected: Boolean) {
    _absFields.addAbsoluteField(f, isSelected)
  }

  def deselectAllFields() {
    _absFields.deselectAllFields()
    selectBottomCropField(false)
    selectRightCropField(false)
    selectTopCropField(false)
    selectLeftCropField(false)
  }

  def selectAbsoluteFields(rect: Rectangle2D, e: MouseEvent) {
    _absFields.selectFields(rect, e)
  }

  def selectCropFields(rect: Rectangle2D, e: MouseEvent) {
    if (! isTopCropFieldSelected) {
      _topCropField.foreach { f =>
        if (f.intersects(rect)) {
          selectTopCropField(true)
        }
      }
    }

    if (! isLeftCropFieldSelected) {
      _leftCropField.foreach { f =>
        if (f.intersects(rect)) {
          selectLeftCropField(true)
        }
      }
    }

    if (! isRightCropFieldSelected) {
      _rightCropField.foreach { f =>
        if (f.intersects(rect)) {
          selectRightCropField(true)
        }
      }
    }

    if (! isBottomCropFieldSelected) {
      _bottomCropField.foreach { f =>
        if (f.intersects(rect)) {
          selectBottomCropField(true)
        }
      }
    }
  }

  def selectSingleFieldAt(x: Double, y: Double) {
    if (_absFields.selectSingleAbsoluteFieldAt(x, y).isEmpty) {
      leftCropField.find { _.drawArea.contains(x, y) }.orElse {
        topCropField.find { _.drawArea.contains(x, y) }.orElse {
          rightCropField.find { _.drawArea.contains(x, y) }.orElse {
            bottomCropField.find { _.drawArea.contains(x, y) }
          }
        }
      }.foreach { cf =>
        projectContext.onSelectedCropFieldAdded(cf)
      }
    }
  }

  def selectField(f: Field) {
    f match {
      case af: AbsoluteField => selectAbsoluteField(af)
      case cf: CropField => selectCropField(cf)
    }
  }

  def selectCropField(f: CropField) {
    f match {
      case lf: LeftCropField => selectLeftCropField(true)
      case rf: RightCropField => selectRightCropField(true)
      case tf: TopCropField => selectTopCropField(true)
      case bf: BottomCropField => selectBottomCropField(true)
    }
  }

  def selectAbsoluteField(f: AbsoluteField) {
    _absFields.selectAbsoluteField(f)
  }

  def deleteAllSelectedFields() {
    _absFields.deleteAllSelectedFields()
  }

  def getSelectedFieldAt(x: Double, y: Double): Option[Field] =
    getSelectedAbsoluteFieldAt(x, y).orElse(getSelectedCropFieldAt(x, y))

  def getSelectedAbsoluteFieldAt(x: Double, y: Double): Option[AbsoluteField] =
    _absFields.getSelectedFieldAt(x, y)

  def getCropFieldAt(x: Double, y: Double, isSelected: Boolean): Option[CropField] =
    _leftCropField.filter(isSelected == isLeftCropFieldSelected && _.contains(x, y)).orElse(
      _rightCropField.filter(isSelected == isRightCropFieldSelected && _.contains(x, y))
    ).orElse(
      _topCropField.filter(isSelected == isTopCropFieldSelected && _.contains(x, y))
    ).orElse(
      _bottomCropField.filter(isSelected == isBottomCropFieldSelected && _.contains(x, y))
    )

  def getSelectedCropFieldAt(x: Double, y: Double): Option[CropField] = getCropFieldAt(x, y, true)

  def getNormalFieldAt(x: Double, y: Double): Option[Field] =
    getNormalAbsoluteFieldAt(x, y).orElse(getNormalCropFieldAt(x, y))

  def getNormalAbsoluteFieldAt(x: Double, y: Double): Option[AbsoluteField] =
    _absFields.getNormalFieldAt(x, y)

  def getNormalCropFieldAt(x: Double, y: Double): Option[CropField] = getCropFieldAt(x, y, false)

  def moveSelectedFields(from: Point2D, to: Point2D) {
    moveSelectedAbsoluteFields(from, to)
    moveSelectedCropFields(from, to)
  }

  def northResizeSelectedFields(from: Point2D, to: Point2D) {
    northResizeSelectedAbsoluteFields(from, to)
    northResizeSelectedCropFields(from, to)
  }

  def eastResizeSelectedFields(from: Point2D, to: Point2D) {
    eastResizeSelectedAbsoluteFields(from, to)
    eastResizeSelectedCropFields(from, to)
  }

  def westResizeSelectedFields(from: Point2D, to: Point2D) {
    westResizeSelectedAbsoluteFields(from, to)
    westResizeSelectedCropFields(from, to)
  }

  def southResizeSelectedFields(from: Point2D, to: Point2D) {
    southResizeSelectedAbsoluteFields(from, to)
    southResizeSelectedCropFields(from, to)
  }

  def northWestResizeSelectedFields(from: Point2D, to: Point2D) {
    northWestResizeSelectedAbsoluteFields(from, to)
    northWestResizeSelectedCropFields(from, to)
  }

  def northEastResizeSelectedFields(from: Point2D, to: Point2D) {
    northEastResizeSelectedAbsoluteFields(from, to)
    northEastResizeSelectedCropFields(from, to)
  }

  def southWestResizeSelectedFields(from: Point2D, to: Point2D) {
    southWestResizeSelectedAbsoluteFields(from, to)
    southWestResizeSelectedCropFields(from, to)
  }

  def southEastResizeSelectedFields(from: Point2D, to: Point2D) {
    southEastResizeSelectedAbsoluteFields(from, to)
    southEastResizeSelectedCropFields(from, to)
  }

  def moveSelectedAbsoluteFields(from: Point2D, to: Point2D) {
    _absFields.moveSelectedFields(from, to)
  }

  def modifySelectedCropField[T <: CropField](
    field: Option[T], isSelected: Boolean, modifier: T => T
  ) {
    field.foreach { f =>
      if (isSelected) {
        val modified = modifier(f)
        addCropField(modified, true)
        projectContext.onSelectedCropFieldRemoved(f)
        projectContext.onSelectedCropFieldAdded(modified)
      }
    }
  }

  def moveSelectedCropFields(from: Point2D, to: Point2D) {
    val modifier: CropField => CropField = _.move(from, to)
    modifySelectedCropField(topCropField, isTopCropFieldSelected, modifier)
    modifySelectedCropField(leftCropField, isLeftCropFieldSelected, modifier)
    modifySelectedCropField(rightCropField, isRightCropFieldSelected, modifier)
    modifySelectedCropField(bottomCropField, isBottomCropFieldSelected, modifier)
  }

  def northResizeSelectedAbsoluteFields(from: Point2D, to: Point2D) {
    _absFields.northResizeSelectedFields(from, to)
  }

  def eastResizeSelectedAbsoluteFields(from: Point2D, to: Point2D) {
    _absFields.eastResizeSelectedFields(from, to)
  }

  def westResizeSelectedAbsoluteFields(from: Point2D, to: Point2D) {
    _absFields.westResizeSelectedFields(from, to)
  }

  def southResizeSelectedAbsoluteFields(from: Point2D, to: Point2D) {
    _absFields.southResizeSelectedFields(from, to)
  }

  def northEastResizeSelectedAbsoluteFields(from: Point2D, to: Point2D) {
    _absFields.northEastResizeSelectedFields(from, to)
  }

  def northWestResizeSelectedAbsoluteFields(from: Point2D, to: Point2D) {
    _absFields.northWestResizeSelectedFields(from, to)
  }

  def southEastResizeSelectedAbsoluteFields(from: Point2D, to: Point2D) {
    _absFields.southEastResizeSelectedFields(from, to)
  }

  def southWestResizeSelectedAbsoluteFields(from: Point2D, to: Point2D) {
    _absFields.southWestResizeSelectedFields(from, to)
  }

  def northResizeSelectedCropFields(from: Point2D, to: Point2D) {
    val modifier: CropField => CropField = _.northResize(from, to)
    modifySelectedCropField(topCropField, isTopCropFieldSelected, modifier)
    modifySelectedCropField(leftCropField, isLeftCropFieldSelected, modifier)
    modifySelectedCropField(rightCropField, isRightCropFieldSelected, modifier)
    modifySelectedCropField(bottomCropField, isBottomCropFieldSelected, modifier)
  }

  def eastResizeSelectedCropFields(from: Point2D, to: Point2D) {
    val modifier: CropField => CropField = _.eastResize(from, to)
    modifySelectedCropField(topCropField, isTopCropFieldSelected, modifier)
    modifySelectedCropField(leftCropField, isLeftCropFieldSelected, modifier)
    modifySelectedCropField(rightCropField, isRightCropFieldSelected, modifier)
    modifySelectedCropField(bottomCropField, isBottomCropFieldSelected, modifier)
  }

  def westResizeSelectedCropFields(from: Point2D, to: Point2D) {
    val modifier: CropField => CropField = _.westResize(from, to)
    modifySelectedCropField(topCropField, isTopCropFieldSelected, modifier)
    modifySelectedCropField(leftCropField, isLeftCropFieldSelected, modifier)
    modifySelectedCropField(rightCropField, isRightCropFieldSelected, modifier)
    modifySelectedCropField(bottomCropField, isBottomCropFieldSelected, modifier)
  }

  def southResizeSelectedCropFields(from: Point2D, to: Point2D) {
    val modifier: CropField => CropField = _.southResize(from, to)
    modifySelectedCropField(topCropField, isTopCropFieldSelected, modifier)
    modifySelectedCropField(leftCropField, isLeftCropFieldSelected, modifier)
    modifySelectedCropField(rightCropField, isRightCropFieldSelected, modifier)
    modifySelectedCropField(bottomCropField, isBottomCropFieldSelected, modifier)
  }

  def northEastResizeSelectedCropFields(from: Point2D, to: Point2D) {
    val modifier: CropField => CropField = _.northEastResize(from, to)
    modifySelectedCropField(topCropField, isTopCropFieldSelected, modifier)
    modifySelectedCropField(leftCropField, isLeftCropFieldSelected, modifier)
    modifySelectedCropField(rightCropField, isRightCropFieldSelected, modifier)
    modifySelectedCropField(bottomCropField, isBottomCropFieldSelected, modifier)
  }

  def northWestResizeSelectedCropFields(from: Point2D, to: Point2D) {
    val modifier: CropField => CropField = _.northWestResize(from, to)
    modifySelectedCropField(topCropField, isTopCropFieldSelected, modifier)
    modifySelectedCropField(leftCropField, isLeftCropFieldSelected, modifier)
    modifySelectedCropField(rightCropField, isRightCropFieldSelected, modifier)
    modifySelectedCropField(bottomCropField, isBottomCropFieldSelected, modifier)
  }

  def southEastResizeSelectedCropFields(from: Point2D, to: Point2D) {
    val modifier: CropField => CropField = _.southEastResize(from, to)
    modifySelectedCropField(topCropField, isTopCropFieldSelected, modifier)
    modifySelectedCropField(leftCropField, isLeftCropFieldSelected, modifier)
    modifySelectedCropField(rightCropField, isRightCropFieldSelected, modifier)
    modifySelectedCropField(bottomCropField, isBottomCropFieldSelected, modifier)
  }

  def southWestResizeSelectedCropFields(from: Point2D, to: Point2D) {
    val modifier: CropField => CropField = _.southWestResize(from, to)
    modifySelectedCropField(topCropField, isTopCropFieldSelected, modifier)
    modifySelectedCropField(leftCropField, isLeftCropFieldSelected, modifier)
    modifySelectedCropField(rightCropField, isRightCropFieldSelected, modifier)
    modifySelectedCropField(bottomCropField, isBottomCropFieldSelected, modifier)
  }

  def renameSelectedAbsoluteField(f: AbsoluteField, newName: String) {
    _absFields.renameSelectedField(f, newName)
  }

  def onCropFieldRemoved(f: CropField, isSelected: Boolean) {
    if (isSelected) {
      projectContext.onSelectedCropFieldRemoved(f)
    }
    else {
      projectContext.onNormalCropFieldRemoved(f)
    }
  }

  def onCropFieldAdded(f: CropField, isSelected: Boolean) {
    if (isSelected) {
      projectContext.onSelectedCropFieldAdded(f)
    }
    else {
      projectContext.onNormalCropFieldAdded(f)
    }
  }

  def redrawCropField(f: CropField, isSelected: Boolean) {
    projectContext.redrawCropField(f)
  }

  def redraw() {
    _absFields.redraw()
    if (! cropEnabled) {
      topCropField.foreach { f =>
        redrawCropField(f, isTopCropFieldSelected)
      }
      leftCropField.foreach { f =>
        redrawCropField(f, isLeftCropFieldSelected)
      }
      rightCropField.foreach { f =>
        redrawCropField(f, isRightCropFieldSelected)
      }
      bottomCropField.foreach { f =>
        redrawCropField(f, isBottomCropFieldSelected)
      }
    }
  }

  def possibleMouseOperation(x: Double, y: Double): MouseOperation = {
    var ret = _absFields.possibleMouseOperation(x, y)
    if (ret != CanDoNothing) return ret

    ret = topCropField.map { _.possibleMouseOperation(x, y) }.getOrElse(CanDoNothing)
    if (ret != CanDoNothing) return ret

    ret = leftCropField.map { _.possibleMouseOperation(x, y) }.getOrElse(CanDoNothing)
    if (ret != CanDoNothing) return ret

    ret = rightCropField.map { _.possibleMouseOperation(x, y) }.getOrElse(CanDoNothing)
    if (ret != CanDoNothing) return ret

    ret = bottomCropField.map { _.possibleMouseOperation(x, y) }.getOrElse(CanDoNothing)
    if (ret != CanDoNothing) return ret

    CanDoNothing
  }

  // returns old field
  def addLeftCropField(f: LeftCropField, selected: Boolean): Option[LeftCropField] = {
    val existing = this._leftCropField
    this._leftCropField = Some(f)
    this._isLeftCropFieldSelected = selected
    existing.foreach { f => onCropFieldRemoved(f, isLeftCropFieldSelected) }
    onCropFieldAdded(f, selected)
    existing
  }

  // returns old field
  def addRightCropField(f: RightCropField, selected: Boolean): Option[RightCropField] = {
    val existing = this._rightCropField
    this._rightCropField = Some(f)
    this._isRightCropFieldSelected = selected
    existing.foreach { f => onCropFieldRemoved(f, isRightCropFieldSelected) }
    onCropFieldAdded(f, selected)
    existing
  }

  // returns old field
  def addTopCropField(f: TopCropField, selected: Boolean): Option[TopCropField] = {
    val existing = this._topCropField
    this._topCropField = Some(f)
    this._isTopCropFieldSelected = selected
    existing.foreach { f => onCropFieldRemoved(f, isTopCropFieldSelected) }
    onCropFieldAdded(f, selected)
    existing
  }

  // returns old field
  def addBottomCropField(f: BottomCropField, selected: Boolean): Option[BottomCropField] = {
    val existing = this._bottomCropField
    this._bottomCropField = Some(f)
    this._isBottomCropFieldSelected = selected
    existing.foreach { f => onCropFieldRemoved(f, isBottomCropFieldSelected) }
    onCropFieldAdded(f, selected)
    existing
  }

  def addCropField[T <: CropField](f: T, selected: Boolean): Option[CropField] = f match {
    case top: TopCropField => addTopCropField(top, selected)
    case left: LeftCropField => addLeftCropField(left, selected)
    case right: RightCropField => addRightCropField(right, selected)
    case bottom: BottomCropField => addBottomCropField(bottom, selected)
  }

  def leftCropField: Option[LeftCropField] = _leftCropField
  def topCropField: Option[TopCropField] = _topCropField
  def rightCropField: Option[RightCropField] = _rightCropField
  def bottomCropField: Option[BottomCropField] = _bottomCropField

  def isTopCropFieldSelected: Boolean = _isTopCropFieldSelected
  def isLeftCropFieldSelected: Boolean = _isLeftCropFieldSelected
  def isRightCropFieldSelected: Boolean = _isRightCropFieldSelected
  def isBottomCropFieldSelected: Boolean = _isBottomCropFieldSelected

  def selectLeftCropField(selected: Boolean) {
    leftCropField.foreach { cf =>
      _isLeftCropFieldSelected = selected
      projectContext.selectCropField(cf, selected)
    }
  }

  def selectTopCropField(selected: Boolean) {
    topCropField.foreach { cf =>
      _isTopCropFieldSelected = selected
      projectContext.selectCropField(cf, selected)
    }
  }

  def selectRightCropField(selected: Boolean) {
    rightCropField.foreach { cf =>
      _isRightCropFieldSelected = selected
      projectContext.selectCropField(cf, selected)
    }
  }

  def selectBottomCropField(selected: Boolean) {
    bottomCropField.foreach { cf =>
      _isBottomCropFieldSelected = selected
      projectContext.selectCropField(cf, selected)
    }
  }

  def cropEnabled_=(enabled: Boolean) {
    _isEdgeCropEnabled = enabled
    listener.onCropEnabledChanged(enabled)
  }

  def cropEnabled: Boolean = _isEdgeCropEnabled

  def edgeCrop(formWidth: Double, formHeight: Double): EdgeCrop = {
    def cropFieldToArea(f: CropField): Area = {
      val rect = f.rect
      Area(
        Percent(100 * rect.minX / formWidth), Percent(100 * rect.minY / formHeight),
        Percent(100 * rect.width / formWidth), Percent(100 * rect.height / formHeight)
      )
    }

    EdgeCrop(
      _isEdgeCropEnabled,
      EdgeCropConditionImpl(
        _topCropField.map(cropFieldToArea),
        _bottomCropField.map(cropFieldToArea),
        _leftCropField.map(cropFieldToArea),
        _rightCropField.map(cropFieldToArea)
      )
    )
  }

  override def cachedImage(
    selectedImage: SelectedImage,
    isSkewCorrectionEnabled: Boolean = skewCorrection.enabled,
    isCropEnabled: Boolean = cropEnabled
  ): Either[(Option[PrepareResult], Image), Future[(Option[PrepareResult], Image)]] = {
    println("cachedImage isSkewCorrectionEnabled = " + isSkewCorrectionEnabled + ", isCropEnabled = " + isCropEnabled)

    val key = (selectedImage.file, isSkewCorrectionEnabled, isCropEnabled) 
    _cachedImage.get(key) match {
      case Some(img) => Left(img)
      case None => {
        val fimg = retrievePreparedImage(selectedImage, isSkewCorrectionEnabled, isCropEnabled)
        Right(
          fimg.map { img =>
            _cachedImage = _cachedImage.updated(key, img)
            img
          }
        )
      }
    }
  }

  private def extention(path: Path): Option[String] = {
    val fname = path.getFileName().toString()
    val idx = fname.lastIndexOf(".")
    if (idx == -1) None else Some(fname.substring(idx))
  }

  private def prepareFileForCaptureApi(image: SelectedImage): Future[Path] = {
    println("prepareFileForCaptureApi()")

    val configFile = Files.createTempFile(null, null)
    val fileName = "image001" + extention(image.file).getOrElse("")
    val cropTarget: Future[(Double, Double)] = if (skewCorrection.enabled) {
      val fimg = cachedImage(image, true, false).fold(Future.successful, identity)
      fimg.map { img =>
        (img._2.getWidth(), img._2.getHeight())
      }
    }
    else {
      Future.successful(image.image.getWidth() -> image.image.getHeight())
    }
    val captureTarget: Future[(Double, Double)] = {
      val fimg = cachedImage(image).fold(Future.successful, identity)
      fimg.map { img => (img._2.getWidth(), img._2.getHeight()) }
    }

    val json: Future[JsObject] =
      cropTarget.flatMap { cropt =>
        captureTarget.map { capt =>
          Json.obj(
            "inputFiles" -> JsArray(Seq(JsString(fileName))),
            "skewCorrection" -> skewCorrection.asJson(skewCorrection.enabled),
            "crop" -> edgeCrop(cropt._1, cropt._2).asJson(cropEnabled),
            "absoluteFields" -> _absFields.asJson(capt._1, capt._2)
          )
        }
      }

    json.map { js =>
      Files.write(configFile, js.toString.getBytes("utf-8"))

      val zipFile = Files.createTempFile(null, null)
      Zip.deflate(
        zipFile,
        Seq(
          "config.json" -> configFile,
          fileName -> image.file
        )
      )

      Files.delete(configFile)

      zipFile
    }
  }

  private def prepareFileForPrepareApi(
    image: SelectedImage,
    isSkewCorrectionEnabled: Boolean,
    isCropEnabled: Boolean
  ): Future[Path] = {
    println("prepareFileForPrepareApi()")

    val configFile = Files.createTempFile(null, null)
    val fileName = "image001" + extention(image.file).getOrElse("")
    val cropTarget: Future[(Double, Double)] = if (! isCropEnabled) {
      Future.successful(0d -> 0d)
    }
    else if (skewCorrection.enabled) {
      cachedImage(image, true, false).fold(Future.successful, identity).map { i =>
        i._2.getWidth() -> i._2.getHeight()
      }
    }
    else {
      Future.successful(image.image.getWidth() -> image.image.getHeight())
    }

    cropTarget.map { cropt =>
      val json = Json.obj(
        "inputFiles" -> JsArray(Seq(JsString(fileName))),
        "skewCorrection" -> skewCorrection.asJson(isSkewCorrectionEnabled),
        "crop" -> edgeCrop(cropt._1, cropt._2).asJson(isCropEnabled)
      )
      println("Json to send: " + json)
      Files.write(configFile, json.toString.getBytes("utf-8"))

      val zipFile = Files.createTempFile(null, null)
      Zip.deflate(
        zipFile,
        Seq(
          "config.json" -> configFile,
          fileName -> image.file
        )
      )

      Files.delete(configFile)

      zipFile
    }
  }

  def runCapture(si: SelectedImage): Future[CaptureResponse] = {
    prepareFileForCaptureApi(si).map { fileToSubmit =>
      val url = new URL("http://localhost:9000/capture")

      using(url.openConnection().asInstanceOf[HttpURLConnection]) { conn =>
        conn.setDoOutput(true)
        conn.setDoInput(true)
        conn.setRequestMethod("POST")
        conn.setRequestProperty("Content-Type", "application/zip")
        using(conn.getOutputStream) { os =>
          os.write(Files.readAllBytes(fileToSubmit))
        }

        val statusCode = conn.getResponseCode
        println("status = " + statusCode)

        val jsonResult: JsValue = using(conn.getInputStream()) { is =>
          Json.parse(is)
        }.get

        println("result: " + jsonResult)
        CaptureResponse.parse(jsonResult)
      }(c => c.disconnect()).get
    }
  }

  def retrievePreparedImage(
    si: SelectedImage,
    isSkewCorrectionEnabled: Boolean,
    isCropEnabled: Boolean
  ): Future[(Option[PrepareResult], Image)] = {
    if (! isSkewCorrectionEnabled && ! isCropEnabled) return Future.successful((None, si.image))
    prepareFileForPrepareApi(si, isSkewCorrectionEnabled, isCropEnabled).map { fileToSubmit =>
      val url = new URL("http://localhost:9000/prepare")

      using(url.openConnection().asInstanceOf[HttpURLConnection]) { conn =>
        conn.setDoOutput(true)
        conn.setDoInput(true)
        conn.setRequestMethod("POST")
        conn.setRequestProperty("Content-Type", "application/zip")
        using(conn.getOutputStream) { os =>
          os.write(Files.readAllBytes(fileToSubmit))
        }

        val statusCode = conn.getResponseCode
        println("status = " + statusCode)

        using(new ZipInputStream(conn.getInputStream())) { zipIn =>
          @tailrec def parseZip(
            resp: Option[PrepareResult], finalImageFileName: Option[String], finalImage: Option[Image]
          ): (Option[PrepareResult], Image) = {
            val entry = zipIn.getNextEntry()
            if (entry == null) (resp, finalImage.get)
            else {
              entry.getName match {
                case "response.json" =>
                  // Json.parse() closes input stream...
                  val json: JsValue = using(Files.createTempFile(null, null)) { tmp =>
                    Files.copy(zipIn, tmp, java.nio.file.StandardCopyOption.REPLACE_EXISTING)
                    zipIn.closeEntry()
                    println("Response json: " + Files.readAllLines(tmp))
                    Json.parse(new FileInputStream(tmp.toFile))
                  }(f => Files.delete(f)).get
                  val prepareResult = PrepareResult.parse(json)
                  val fileName: Option[String] = prepareResult.cropResult.flatMap { cr =>
                    cr match {
                      case CropResultSuccess(correctedFiles) => Some(correctedFiles.head)
                      case CropResultCannotFindEdge => None
                    }
                  }.orElse {
                    prepareResult.skewCorrectionResult.map { scr =>
                      scr.correctedFiles.head
                    }
                  }
                  parseZip(Some(prepareResult), fileName, finalImage)
                case fname: String =>
                  if (fname == finalImageFileName.get) {
                    val i = Some(new Image(zipIn))
                    zipIn.closeEntry()
                    parseZip(resp, finalImageFileName, i)
                  }
                  else {
                    parseZip(resp, finalImageFileName, finalImage)
                  }
              }
            }
          }

          parseZip(None, None, None)
        }.get
      }(c => c.disconnect()).get
    }
  }

  override def invalidateCachedImage(skewCorrected: Boolean = false, cropped: Boolean = false) {
    _cachedImage = _cachedImage.filter { case (key, value) =>
      val shouldDrop = skewCorrected && key._2 || cropped && key._3
      ! shouldDrop
    }
  }

  override def cropFieldsAreReady: Boolean =
    _topCropField.isDefined && _leftCropField.isDefined && _rightCropField.isDefined && _bottomCropField.isDefined
}
