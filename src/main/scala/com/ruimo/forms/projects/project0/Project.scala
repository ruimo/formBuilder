package com.ruimo.forms.projects.project0

import javafx.scene.input.MouseEvent
import javafx.scene.paint.Color

import com.ruimo.forms._
import play.api.libs.json.{JsNumber, JsObject, JsValue, Json}

import scala.collection.{immutable => imm}
import scalafx.geometry.{Point2D, Rectangle2D}
import scalafx.scene.canvas.{GraphicsContext => SfxGraphicsContext}

object AbsoluteFieldImpl {
  val LineWidth = 2.0
}

case class AbsoluteFieldImpl(rect: Rectangle2D, name: String) extends AbsoluteField {
  import AbsoluteFieldImpl._

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

  def move(from: Point2D, to: Point2D): AbsoluteField = copy(
    rect = new Rectangle2D(
      this.rect.minX + (to.x - from.x),
      this.rect.minY + (to.y - from.y),
      this.rect.width,
      this.rect.height
    )
  )

  def withName(newName: String): AbsoluteField =
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
    else
      CanMove(this)
  }
}

object CropFieldImpl {
  val LineWidth = 2.0
}

class CropFieldImpl(rect: Rectangle2D) extends CropField {
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
    else
      CanMove(this)
  }

  override def toLeft: LeftCropField = LeftCropFieldImpl(rect)
  override def toRight: RightCropField = RightCropFieldImpl(rect)
  override def toBottom: BottomCropField = BottomCropFieldImpl(rect)
  override def toTop: TopCropField = TopCropFieldImpl(rect)
}

case class TopCropFieldImpl(rect: Rectangle2D) extends CropFieldImpl(rect) with TopCropField {
  override def toLeft: LeftCropField = LeftCropFieldImpl(rect)
  override def toRight: RightCropField = RightCropFieldImpl(rect)
  override def toBottom: BottomCropField = BottomCropFieldImpl(rect)
  override def toTop: TopCropField = this
}
case class LeftCropFieldImpl(rect: Rectangle2D) extends CropFieldImpl(rect) with LeftCropField {
  override def toLeft: LeftCropField = this
  override def toRight: RightCropField = RightCropFieldImpl(rect)
  override def toBottom: BottomCropField = BottomCropFieldImpl(rect)
  override def toTop: TopCropField = TopCropFieldImpl(rect)
}
case class RightCropFieldImpl(rect: Rectangle2D) extends CropFieldImpl(rect) with RightCropField {
  override def toLeft: LeftCropField = LeftCropFieldImpl(rect)
  override def toRight: RightCropField = this
  override def toBottom: BottomCropField = BottomCropFieldImpl(rect)
  override def toTop: TopCropField = TopCropFieldImpl(rect)
}
case class BottomCropFieldImpl(rect: Rectangle2D) extends CropFieldImpl(rect) with BottomCropField {
  override def toLeft: LeftCropField = LeftCropFieldImpl(rect)
  override def toRight: RightCropField = RightCropFieldImpl(rect)
  override def toBottom: BottomCropField = this
  override def toTop: TopCropField = TopCropFieldImpl(rect)
}

case class SkewCorrectionImpl(
  enabled: Boolean,
  direction: SkewCorrectionDirection = SkewCorrectionDirectionHorizontal,
  lineCount: Int = 1,
  maxAngleToDetect: Double = 2.0
) extends SkewCorrection {
  def withEnabled(newEnabled: Boolean): SkewCorrection = copy(
    enabled = newEnabled
  )

  lazy val asJson: JsValue = Json.obj(
    "enabled" -> enabled,
    "direction" -> direction.asJson,
    "lineCount" -> JsNumber(lineCount),
    "maxAngleToDetect" -> JsNumber(maxAngleToDetect)
  )
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
  private[this] var skCorrection: SkewCorrection = SkewCorrectionImpl(false)
  private[this] var absFields = new AbsoluteFieldTable(projectContext)
  private[this] var _leftCropField: Option[LeftCropField] = None
  private[this] var _rightCropField: Option[RightCropField] = None
  private[this] var _topCropField: Option[TopCropField] = None
  private[this] var _bottomCropField: Option[BottomCropField] = None
  private[this] var _isLeftCropFieldSelected: Boolean = false
  private[this] var _isRightCropFieldSelected: Boolean = false
  private[this] var _isTopCropFieldSelected: Boolean = false
  private[this] var _isBottomCropFieldSelected: Boolean = false

  def withListener(newListener: ProjectListener): Project = new ProjectImpl(
    this.projectContext,
    newListener
  )

  def absoluteFields: AbsoluteFieldTable = absFields

  def skewCorrection: SkewCorrection = skCorrection

  def skewCorrection_=(newSkewCorrection: SkewCorrection) {
    if (newSkewCorrection != skCorrection) {
      this.skCorrection = newSkewCorrection
      listener.onSkewCorrectionChanged(newSkewCorrection)
    }
  }
  
  def addAbsoluteField(f: AbsoluteField, isSelected: Boolean) {
    absFields.addAbsoluteField(f, isSelected)
  }

  def deselectAllFields() {
    absFields.deselectAllFields()
  }

  def selectAbsoluteFields(rect: Rectangle2D, e: MouseEvent) {
    absFields.selectAbsoluteFields(rect, e)
  }

  def selectSingleFieldAt(x: Double, y: Double) {
    if (absFields.selectSingleAbsoluteFieldAt(x, y).isEmpty) {
      leftCropField.find { _.drawArea.contains(x, y) }.orElse {
        topCropField.find { _.drawArea.contains(x, y) }.orElse {
          rightCropField.find { _.drawArea.contains(x, y) }.orElse {
            bottomCropField.find { _.drawArea.contains(x, y) }
          }
        }
      }.foreach { cf =>
        projectContext.onCropFieldAdded(cf)
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
    absFields.selectAbsoluteField(f)
  }

  def deleteAllSelectedFields() {
    absFields.deleteAllSelectedFields()
  }

  def getSelectedFieldAt(x: Double, y: Double): Option[Field] =
    getSelectedAbsoluteFieldAt(x, y).orElse(getSelectedCropFieldAt(x, y))

  def getSelectedAbsoluteFieldAt(x: Double, y: Double): Option[AbsoluteField] =
    absFields.getSelectedFieldAt(x, y)

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
    absFields.getNormalFieldAt(x, y)

  def getNormalCropFieldAt(x: Double, y: Double): Option[CropField] = getCropFieldAt(x, y, false)

  def moveSelectedAbsoluteFields(from: Point2D, to: Point2D) {
    absFields.moveSelectedAbsoluteFields(from, to)
  }

  def renameSelectedAbsoluteField(f: AbsoluteField, newName: String) {
    absFields.renameSelectedAbsoluteField(f, newName)
  }

  def redraw() {
    absFields.redraw()
  }

  def possibleMouseOperation(x: Double, y: Double): MouseOperation =
    absFields.possibleMouseOperation(x, y)

  def northResize(f: Field, p0: Point2D, p1: Point2D) {
    f match {
      case af: AbsoluteField =>
        absFields.northResize(af, p0, p1)
    }
  }

  // returns old field
  def addLeftCropField(f: LeftCropField, selected: Boolean): Option[LeftCropField] = {
    val existing = this._leftCropField
    this._leftCropField = Some(f)
    this._isLeftCropFieldSelected = selected
    existing
  }

  // returns old field
  def addRightCropField(f: RightCropField, selected: Boolean): Option[RightCropField] = {
    val existing = this._rightCropField
    this._rightCropField = Some(f)
    this._isRightCropFieldSelected = selected
    existing
  }

  // returns old field
  def addTopCropField(f: TopCropField, selected: Boolean): Option[TopCropField] = {
    val existing = this._topCropField
    this._topCropField = Some(f)
    this._isTopCropFieldSelected = selected
    existing
  }

  // returns old field
  def addBottomCropField(f: BottomCropField, selected: Boolean): Option[BottomCropField] = {
    val existing = this._bottomCropField
    this._bottomCropField = Some(f)
    this._isBottomCropFieldSelected = selected
    existing
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
      projectContext.onCropFieldAdded(cf)
    }
  }
  def selectTopCropField(selected: Boolean) {
    topCropField.foreach { cf =>
      _isTopCropFieldSelected = selected
      projectContext.onCropFieldAdded(cf)
    }
  }
  def selectRightCropField(selected: Boolean) {
    rightCropField.foreach { cf =>
      _isRightCropFieldSelected = selected
      projectContext.onCropFieldAdded(cf)
    }
  }
  def selectBottomCropField(selected: Boolean) {
    bottomCropField.foreach { cf =>
      _isBottomCropFieldSelected = selected
      projectContext.onCropFieldAdded(cf)
    }
  }
}
