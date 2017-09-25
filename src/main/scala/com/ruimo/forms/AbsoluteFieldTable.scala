package com.ruimo.forms

import javafx.scene.input.MouseEvent

import play.api.libs.json._

import scala.collection.{immutable => imm}
import scalafx.geometry.{Point2D, Rectangle2D}

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

  def asJson(width: Double, height: Double): JsValue = JsArray(
    (normalFields ++ selectedFields).map { f =>
      JsObject(
        Seq(
          "name" -> JsString(f.name),
          "rect" -> JsArray(
            imm.Seq(
              JsNumber(100 * f.rect.minX / width),
              JsNumber(100 * f.rect.minY / height),
              JsNumber(100 * f.rect.width / width),
              JsNumber(100 * f.rect.height / height)
            )
          )
        )
      )
    }
  )
}

