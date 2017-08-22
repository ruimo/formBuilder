package com.ruimo.forms

import scalafx.geometry.{Point2D, Rectangle2D}
import javafx.scene.input.MouseEvent

import com.ruimo.graphics.twodim.Area

import scalafx.scene.canvas.{GraphicsContext => SfxGraphicsContext}
import scala.collection.{immutable => imm}

sealed trait SkewCorrectionDirection
case object SkewCorrectionDirectionHorizontal extends SkewCorrectionDirection
case object SkewCorrectionDirectionVertical extends SkewCorrectionDirection

trait Field extends Widget[Field] {
  def drawArea: Rectangle2D
  def draw(gc: SfxGraphicsContext, isSelected: Boolean): Unit
}


trait SkewCorrection {
  def enabled: Boolean
  def direction: SkewCorrectionDirection
  def lineCount: Int
  def maxAngleToDetect: Double

  def withEnabled(newEnabled: Boolean): SkewCorrection
}

trait EdgeCrop {
  def enabled: Boolean
  def topArea: Option[Area]
  def bottomArea: Option[Area]
  def leftArea: Option[Area]
  def rightArea: Option[Area]
}

trait AbsoluteField extends Field {
  def move(from: Point2D, to: Point2D): AbsoluteField
  def name: String
  def withName(newName: String): AbsoluteField
  def possibleMouseOperation(x: Double, y: Double): MouseOperation
}

trait CropField extends Field {
  def toLeft: LeftCropField
  def toTop: TopCropField
  def toRight: RightCropField
  def toBottom: BottomCropField
}
trait LeftCropField extends CropField
trait TopCropField extends CropField
trait RightCropField extends CropField
trait BottomCropField extends CropField

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

  def selectSingleAbsoluteFieldAt(x: Double, y: Double): Option[Field] = {
    val found = _normalFields.zipWithIndex.find { case (f, idx) =>
      f.contains(x, y)
    }

    found.foreach { case (f, idx) =>
        val sp = _normalFields.splitAt(idx)
        _normalFields = sp._1 ++ sp._2.tail
        _selectedFields = _selectedFields :+ f
        projectContext.onSelectedAbsoluteFieldAdded(f)
        projectContext.onNormalAbsoluteFieldRemoved(f)
    }

    found.map(_._1)
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

trait ProjectListener {
  def onSkewCorrectionChanged(skewCorrection: SkewCorrection)
}

object NullObjectListener extends ProjectListener {
  def onSkewCorrectionChanged(skewCorrection: SkewCorrection) {}
}

trait Project {
  val version: Version
  def listener: ProjectListener

  def withListener(newListener: ProjectListener): Project
  def addAbsoluteField(f: AbsoluteField, isSelected: Boolean): Unit
  def absoluteFields: AbsoluteFieldTable
  def deselectAllFields(): Unit
  def selectAbsoluteFields(rect: Rectangle2D, e: MouseEvent): Unit
  def selectSingleFieldAt(x: Double, y: Double): Unit
  def selectAbsoluteField(f: AbsoluteField): Unit
  def deleteAllSelectedFields(): Unit
  def getSelectedAbsoluteField(x: Double, y: Double): Option[AbsoluteField]
  def getNormalAbsoluteField(x: Double, y: Double): Option[AbsoluteField]
  def moveSelectedAbsoluteFields(from: Point2D, to: Point2D): Unit
  def renameSelectedAbsoluteField(f: AbsoluteField, newName: String): Unit
  def redraw(): Unit
  def possibleMouseOperation(x: Double, y: Double): MouseOperation
  def northResize(f: Field, p0: Point2D, p1: Point2D)
  def skewCorrection: SkewCorrection
  def skewCorrection_=(newSkewCorrection: SkewCorrection)

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
  val onCropFieldAdded: CropField => Unit
)
