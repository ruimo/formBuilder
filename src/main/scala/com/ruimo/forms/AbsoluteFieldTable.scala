package com.ruimo.forms

import javafx.scene.input.MouseEvent

import com.ruimo.forms.common.OcrSettings
import play.api.libs.json._

import scala.collection.{immutable => imm}
import scalafx.geometry.{Point2D, Rectangle2D}

class AbsoluteFieldTable(
  projectContext: ProjectContext
) {
  @volatile
  private[this] var _normalAbsoluteFields: imm.Seq[AbsoluteField] = imm.Seq()
  @volatile
  private[this] var _selectedAbsoluteFields: imm.Seq[AbsoluteField] = imm.Seq()

  def normalFields: imm.Seq[AbsoluteField] = _normalAbsoluteFields
  def selectedFields: imm.Seq[AbsoluteField] = _selectedAbsoluteFields

  def isAbsoluteFIeldNameAlreadyUsed(name: String): Boolean =
    normalFields.find(_.name == name) != None || selectedFields.find(_.name == name) != None

  def assumeNewName(f: AbsoluteField) {
    if (isAbsoluteFIeldNameAlreadyUsed(f.name)) {
      throw new AbsoluteFieldTable.DuplicatedNameException(f.name)
    }
  }

  def addAbsoluteField(f: AbsoluteField, isSelected: Boolean, redraw: Boolean = true) {
    assumeNewName(f)

    if (isSelected)
      _selectedAbsoluteFields= _selectedAbsoluteFields :+ f
    else
      _normalAbsoluteFields = _normalAbsoluteFields :+ f

    if (redraw) {
      (if (isSelected) projectContext.onSelectedAbsoluteFieldAdded else projectContext.onNormalAbsoluteFieldAdded).apply(f)
    }
  }

  def deselectAllFields() {
    selectedFields.foreach { af =>
      _normalAbsoluteFields  = _normalAbsoluteFields :+ af
      projectContext.onSelectedAbsoluteFieldRemoved(af)
      projectContext.onNormalAbsoluteFieldAdded(af)
    }

    _selectedAbsoluteFields = imm.Seq()
  }

  def selectFields(formSize: (Double, Double), rect: Rectangle2D, e: MouseEvent) {
    _normalAbsoluteFields = _normalAbsoluteFields.filter { f =>
      if (f.intersects(formSize, rect)) {
        _selectedAbsoluteFields = _selectedAbsoluteFields :+ f
        projectContext.onSelectedAbsoluteFieldAdded(f)
        projectContext.onNormalAbsoluteFieldRemoved(f)
        false
      } else true
    }
  }

  def selectSingleAbsoluteFieldAt(formSize: (Double, Double), x: Double, y: Double): Option[Field] = {
    val found = _normalAbsoluteFields.zipWithIndex.find { case (f, idx) =>
      f.contains(formSize, x, y)
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

  def deleteAllFields() {
    deleteAllSelectedFields()
    deleteAllNonSelectedFields()
  }

  def deleteAllNonSelectedFields() {
    val buf = _normalAbsoluteFields
    _normalAbsoluteFields = imm.Seq()
    buf.foreach { f =>
      projectContext.onSelectedAbsoluteFieldRemoved(f)
    }
  }

  def deleteAllSelectedFields() {
    val buf = _selectedAbsoluteFields
    _selectedAbsoluteFields = imm.Seq()
    buf.foreach { f =>
      projectContext.onSelectedAbsoluteFieldRemoved(f)
    }
  }

  def getSelectedFieldAt(formSize: (Double, Double), x: Double, y: Double): Option[AbsoluteField] =
    _selectedAbsoluteFields.find { f =>
      f.contains(formSize, x, y)
    }

  def getNormalFieldAt(formSize: (Double, Double), x: Double, y: Double): Option[AbsoluteField] =
    _normalAbsoluteFields.find { f =>
      f.contains(formSize, x, y)
    }

  def moveSelectedFields(formSize: (Double, Double), from: Point2D, to: Point2D) {
    modifySelectedFields(_.move(formSize, from, to))
  }

  def northResizeSelectedFields(formSize: (Double, Double), from: Point2D, to: Point2D): Unit = {
    modifySelectedFields(_.northResize(formSize, from, to))
  }

  def eastResizeSelectedFields(formSize: (Double, Double), from: Point2D, to: Point2D): Unit = {
    modifySelectedFields(_.eastResize(formSize, from, to))
  }

  def westResizeSelectedFields(formSize: (Double, Double), from: Point2D, to: Point2D): Unit = {
    modifySelectedFields(_.westResize(formSize, from, to))
  }

  def southResizeSelectedFields(formSize: (Double, Double), from: Point2D, to: Point2D) {
    modifySelectedFields(_.southResize(formSize, from, to))
  }

  def northWestResizeSelectedFields(formSize: (Double, Double), from: Point2D, to: Point2D) {
    modifySelectedFields(_.northWestResize(formSize, from, to))
  }

  def northEastResizeSelectedFields(formSize: (Double, Double), from: Point2D, to: Point2D) {
    modifySelectedFields(_.northEastResize(formSize, from, to))
  }

  def southWestResizeSelectedFields(formSize: (Double, Double), from: Point2D, to: Point2D) {
    modifySelectedFields(_.southWestResize(formSize, from, to))
  }

  def southEastResizeSelectedFields(formSize: (Double, Double), from: Point2D, to: Point2D) {
    modifySelectedFields(_.southEastResize(formSize, from, to))
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

  def updateSelectedField(af: AbsoluteField, newName: String, newOcrSettings: Option[OcrSettings]) {
    if (af.name != newName || af.ocrSettings != newOcrSettings) {
      _selectedAbsoluteFields.zipWithIndex.find { _._1 == af } match {
        case Some((f, idx)) =>
          val sp = _selectedAbsoluteFields.splitAt(idx)
          _selectedAbsoluteFields = (sp._1 :+ f.withNewValue(newName = newName, newOcrSettings = newOcrSettings)) ++ sp._2.tail
        case None =>
      }

    }
  }

  def renameSelectedField(af: AbsoluteField, newName: String) {
    updateSelectedField(af, newName, af.ocrSettings)
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

  def possibleMouseOperation(formSize: (Double, Double), x: Double, y: Double): MouseOperation = {
    _selectedAbsoluteFields.find { f =>
      f.drawArea(formSize).contains(x, y)
    }.orElse {
      _normalAbsoluteFields.find { f =>
        f.drawArea(formSize).contains(x, y)
      }
    } match {
      case None => CanDoNothing
      case Some(f) => f.possibleMouseOperation(formSize, x, y)
    }
  }

  import AbsoluteFieldImpl.absoluteFieldFormat

  def asJson: JsValue = JsArray(
    (normalFields ++ selectedFields).map { f =>
      JsObject(
        Seq(
          "name" -> JsString(f.name),
          "field" -> Json.toJson(f)
        )
      )
    }
  )
}

object AbsoluteFieldTable {
  class DuplicatedNameException(fieldName: String, cause: Throwable) extends Exception(
    "'" + fieldName + "' is already existed", cause
  ) {
    def this(fieldName: String) = this(fieldName, null)
  }
}
