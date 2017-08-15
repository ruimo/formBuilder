package com.ruimo.forms

import javafx.scene.input.MouseEvent
import javafx.scene.paint.Color
import scala.collection.{immutable => imm}
import scalafx.geometry.{Rectangle2D, Point2D}
import scalafx.scene.canvas.{GraphicsContext => SfxGraphicsContext}

sealed trait MouseOperation

case object CanDoNothing extends MouseOperation
case class CanMove(f: Field) extends MouseOperation
case class CanNorthResize(f: Field) extends MouseOperation
case class CanEastResize(f: Field) extends MouseOperation
case class CanWestResize(f: Field) extends MouseOperation
case class CanSouthResize(f: Field) extends MouseOperation
case class CanNorthWestResize(f: Field) extends MouseOperation
case class CanNorthEastResize(f: Field) extends MouseOperation
case class CanSouthWestResize(f: Field) extends MouseOperation
case class CanSouthEastResize(f: Field) extends MouseOperation

sealed trait Field extends Widget[Field]

object AbsoluteField {
  val LineWidth = 2.0
}

case class AbsoluteField(rect: Rectangle2D, name: String) extends Widget[AbsoluteField] with Field {
  import AbsoluteField._

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

sealed trait Project {
  val version: Version
  def addAbsoluteField(f: AbsoluteField, isSelected: Boolean): Unit
  def absoluteFields: AbsoluteFieldTable
  def deselectAllFields(): Unit
  def selectAbsoluteFields(rect: Rectangle2D, e: MouseEvent): Unit
  def selectSingleAbsoluteFieldAt(x: Double, y: Double): Unit
  def selectAbsoluteField(f: AbsoluteField): Unit
  def deleteAllSelectedFields(): Unit
  def getSelectedAbsoluteField(x: Double, y: Double): Option[AbsoluteField]
  def getNormalAbsoluteField(x: Double, y: Double): Option[AbsoluteField]
  def moveSelectedAbsoluteFields(from: Point2D, to: Point2D): Unit
  def renameSelectedAbsoluteField(f: AbsoluteField, newName: String): Unit
  def redraw(): Unit
  def possibleMouseOperation(x: Double, y: Double): MouseOperation
  def northResize(f: Field, p0: Point2D, p1: Point2D)
}

class ProjectContext(
  val onNormalAbsoluteFieldAdded: AbsoluteField => Unit,
  val onSelectedAbsoluteFieldAdded: AbsoluteField => Unit,
  val onNormalAbsoluteFieldRemoved: AbsoluteField => Unit,
  val onSelectedAbsoluteFieldRemoved: AbsoluteField => Unit
)

class AbsoluteFieldTable(
  projectContext: ProjectContext
) {
  private[this] var _normalFields: imm.Seq[AbsoluteField] = imm.Seq()
  private[this] var _selectedFields: imm.Seq[AbsoluteField] = imm.Seq()

  def normalFields: imm.Seq[AbsoluteField] = _normalFields
  def selectedFields: imm.Seq[AbsoluteField] = _selectedFields

  def addAbsoluteField(f: AbsoluteField, isSelected: Boolean) {
    if (isSelected)
      _selectedFields= _selectedFields :+ f
    else
      _normalFields = _normalFields :+ f

    (if (isSelected) projectContext.onSelectedAbsoluteFieldAdded else projectContext.onNormalAbsoluteFieldAdded).apply(f)
  }

  def deselectAllFields() {
    selectedFields.foreach { af =>
      _normalFields  = _normalFields :+ af
      projectContext.onSelectedAbsoluteFieldRemoved(af)
      projectContext.onNormalAbsoluteFieldAdded(af)
    }

    _selectedFields = imm.Seq()
  }

  def selectAbsoluteFields(rect: Rectangle2D, e: MouseEvent) {
    _normalFields = _normalFields.filter { f =>
      if (f.intersects(rect)) {
        _selectedFields = _selectedFields :+ f
        projectContext.onSelectedAbsoluteFieldAdded(f)
        projectContext.onNormalAbsoluteFieldRemoved(f)
        false
      } else true
    }
  }

  def selectSingleAbsoluteFieldAt(x: Double, y: Double) {
    _normalFields.zipWithIndex.find { case (f, idx) =>
      f.contains(x, y)
    }.foreach { case (f, idx) =>
        val sp = _normalFields.splitAt(idx)
        _normalFields = sp._1 ++ sp._2.tail
        _selectedFields = _selectedFields :+ f
        projectContext.onSelectedAbsoluteFieldAdded(f)
        projectContext.onNormalAbsoluteFieldRemoved(f)
    }
  }

  def selectAbsoluteField(af: AbsoluteField) {
    val (f, idx) = _normalFields.zipWithIndex.find { _._1 == af }.get

    val sp = _normalFields.splitAt(idx)
    _normalFields = sp._1 ++ sp._2.tail
    _selectedFields = _selectedFields :+ f
    projectContext.onSelectedAbsoluteFieldAdded(f)
    projectContext.onNormalAbsoluteFieldRemoved(f)
  }

  def deleteAllSelectedFields() {
    val buf = _selectedFields
    _selectedFields = imm.Seq()
    buf.foreach { f =>
      projectContext.onSelectedAbsoluteFieldRemoved(f)
    }
  }

  def getSelectedAbsoluteField(x: Double, y: Double): Option[AbsoluteField] =
    _selectedFields.find { f =>
      f.contains(x, y)
    }

  def getNormalAbsoluteField(x: Double, y: Double): Option[AbsoluteField] =
    _normalFields.find { f =>
      f.contains(x, y)
    }

  def moveSelectedAbsoluteFields(from: Point2D, to: Point2D) {
    val orgFields = _selectedFields

    _selectedFields = _selectedFields.map { f =>
      f.move(from, to)
    }

    orgFields.foreach { f =>
      projectContext.onSelectedAbsoluteFieldRemoved(f)
    }

    _selectedFields.foreach { f =>
      projectContext.onSelectedAbsoluteFieldAdded(f)
    }
  }

  def renameSelectedAbsoluteField(af: AbsoluteField, newName: String) {
    if (af.name != newName) {
      val newField = af.withName(newName)
      _selectedFields.zipWithIndex.find { _._1 == af } match {
        case Some((f, idx)) =>
          val sp = _selectedFields.splitAt(idx)
          _selectedFields = (sp._1 :+ f.withName(newName)) ++ sp._2.tail
        case None =>
      }
    }
  }

  def redraw() {
    _normalFields.foreach { f =>
      projectContext.onNormalAbsoluteFieldRemoved(f)
      projectContext.onNormalAbsoluteFieldAdded(f)
    }

    _selectedFields.foreach { f =>
      projectContext.onSelectedAbsoluteFieldRemoved(f)
      projectContext.onSelectedAbsoluteFieldAdded(f)
    }
  }

  def possibleMouseOperation(x: Double, y: Double): MouseOperation = {
    _selectedFields.find { f =>
      f.drawArea.contains(x, y)
    }.orElse {
      _normalFields.find { f =>
        f.drawArea.contains(x, y)
      }
    } match {
      case None => CanDoNothing
      case Some(f) => f.possibleMouseOperation(x, y)
    }
  }

  def northResize(f: AbsoluteField, p0: Point2D, p1: Point2D) {
  }
}


case class ProjectV1(
  projectContext: ProjectContext
) extends Project {
  val version = VersionOne
  private[this] var absFields = new AbsoluteFieldTable(projectContext)

  def absoluteFields: AbsoluteFieldTable = absFields

  def addAbsoluteField(f: AbsoluteField, isSelected: Boolean) {
    absFields.addAbsoluteField(f, isSelected)
  }

  def deselectAllFields() {
    absFields.deselectAllFields()
  }

  def selectAbsoluteFields(rect: Rectangle2D, e: MouseEvent) {
    absFields.selectAbsoluteFields(rect, e)
  }

  def selectSingleAbsoluteFieldAt(x: Double, y: Double) {
    absFields.selectSingleAbsoluteFieldAt(x, y)
  }

  def selectAbsoluteField(f: AbsoluteField) {
    absFields.selectAbsoluteField(f)
  }

  def deleteAllSelectedFields() {
    absFields.deleteAllSelectedFields()
  }

  def getSelectedAbsoluteField(x: Double, y: Double): Option[AbsoluteField] =
    absFields.getSelectedAbsoluteField(x, y)

  def getNormalAbsoluteField(x: Double, y: Double): Option[AbsoluteField] =
    absFields.getNormalAbsoluteField(x, y)

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
}
