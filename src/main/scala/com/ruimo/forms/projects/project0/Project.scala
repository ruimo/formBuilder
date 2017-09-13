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

  def move(from: Point2D, to: Point2D): this.type = copy(
    rect = new Rectangle2D(
      this.rect.minX + (to.x - from.x),
      this.rect.minY + (to.y - from.y),
      this.rect.width,
      this.rect.height
    )
  ).asInstanceOf[this.type]

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

abstract class CropFieldImpl(rect: Rectangle2D) extends CropField {
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
  def withNewRect(rect: Rectangle2D): this.type
  def move(from: Point2D, to: Point2D): this.type = withNewRect(
    new Rectangle2D(
      this.rect.minX + (to.x - from.x),
      this.rect.minY + (to.y - from.y),
      this.rect.width,
      this.rect.height
    )
  ).asInstanceOf[this.type]
}

case class UnknownCropFieldImpl(rect: Rectangle2D) extends CropFieldImpl(rect) {
  def withNewRect(newRect: Rectangle2D): this.type = copy(rect = newRect).asInstanceOf[this.type]
}

case class TopCropFieldImpl(rect: Rectangle2D) extends CropFieldImpl(rect) with TopCropField {
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
  def withNewRect(newRect: Rectangle2D): this.type = copy(rect = newRect).asInstanceOf[this.type]
}

case class LeftCropFieldImpl(rect: Rectangle2D) extends CropFieldImpl(rect) with LeftCropField {
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
  def withNewRect(newRect: Rectangle2D): this.type = copy(rect = newRect).asInstanceOf[this.type]
}

case class RightCropFieldImpl(rect: Rectangle2D) extends CropFieldImpl(rect) with RightCropField {
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
  def withNewRect(newRect: Rectangle2D): this.type = copy(rect = newRect).asInstanceOf[this.type]
}

case class BottomCropFieldImpl(rect: Rectangle2D) extends CropFieldImpl(rect) with BottomCropField {
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
    selectBottomCropField(false)
    selectRightCropField(false)
    selectTopCropField(false)
    selectLeftCropField(false)
  }

  def selectAbsoluteFields(rect: Rectangle2D, e: MouseEvent) {
    absFields.selectFields(rect, e)
  }

  def selectCropFields(rect: Rectangle2D, e: MouseEvent) {
    if (! isTopCropFieldSelected) {
      _topCropField.foreach { f =>
        if (f.intersects(rect)) {
          addTopCropField(f, true)
        }
      }
    }

    if (! isLeftCropFieldSelected) {
      _leftCropField.foreach { f =>
        if (f.intersects(rect)) {
          addLeftCropField(f, true)
        }
      }
    }

    if (! isRightCropFieldSelected) {
      _rightCropField.foreach { f =>
        if (f.intersects(rect)) {
          addRightCropField(f, true)
        }
      }
    }

    if (! isBottomCropFieldSelected) {
      _bottomCropField.foreach { f =>
        if (f.intersects(rect)) {
          addBottomCropField(f, true)
        }
      }
    }
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

  def moveSelectedFields(from: Point2D, to: Point2D) {
    moveSelectedAbsoluteFields(from, to)
    moveSelectedCropFields(from, to)
  }

  def northResize(p0: Point2D, p1: Point2D) {
    northResizeAbsoluteFields(from, to)
    northResizeCropFields(from, to)
  }

  def moveSelectedAbsoluteFields(from: Point2D, to: Point2D) {
    absFields.moveSelectedAbsoluteFields(from, to)
  }

  def moveSelectedCropFields(from: Point2D, to: Point2D) {
    topCropField.foreach { f =>
      if (isTopCropFieldSelected) {
        val moved = f.move(from, to)
        addTopCropField(moved, true)
        projectContext.onSelectedCropFieldRemoved(f)
        projectContext.onSelectedCropFieldAdded(moved)
      }
    }

    leftCropField.foreach { f =>
      if (isLeftCropFieldSelected) {
        val moved = f.move(from, to)
        addLeftCropField(moved, true)
        projectContext.onSelectedCropFieldRemoved(f)
        projectContext.onSelectedCropFieldAdded(moved)
      }
    }

    rightCropField.foreach { f =>
      if (isRightCropFieldSelected) {
        val moved = f.move(from, to)
        addRightCropField(moved, true)
        projectContext.onSelectedCropFieldRemoved(f)
        projectContext.onSelectedCropFieldAdded(moved)
      }
    }

    bottomCropField.foreach { f =>
      if (isBottomCropFieldSelected) {
        val moved = f.move(from, to)
        addBottomCropField(moved, true)
        projectContext.onSelectedCropFieldRemoved(f)
        projectContext.onSelectedCropFieldAdded(moved)
      }
    }
  }

  def northResizeAbsoluteFields(from: Point2D, to: Point2D) {
    absFields.northResizeFields(from, to)
  }

  def northResizeCropFields(from: Point2D, to: Point2D) {
  }

  def renameSelectedAbsoluteField(f: AbsoluteField, newName: String) {
    absFields.renameSelectedAbsoluteField(f, newName)
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
    if (isSelected) {
      projectContext.onSelectedCropFieldRemoved(f)
      projectContext.onSelectedCropFieldAdded(f)
    }
    else {
      projectContext.onNormalCropFieldRemoved(f)
      projectContext.onNormalCropFieldAdded(f)
    }
  }

  def redraw() {
    absFields.redraw()
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

  def possibleMouseOperation(x: Double, y: Double): MouseOperation = {
    var ret = absFields.possibleMouseOperation(x, y)
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
      onCropFieldAdded(cf, selected)
    }
  }

  def selectTopCropField(selected: Boolean) {
    topCropField.foreach { cf =>
      _isTopCropFieldSelected = selected
      onCropFieldAdded(cf, selected)
    }
  }

  def selectRightCropField(selected: Boolean) {
    rightCropField.foreach { cf =>
      _isRightCropFieldSelected = selected
      onCropFieldAdded(cf, selected)
    }
  }

  def selectBottomCropField(selected: Boolean) {
    bottomCropField.foreach { cf =>
      _isBottomCropFieldSelected = selected
      onCropFieldAdded(cf, selected)
    }
  }
}
