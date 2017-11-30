package com.ruimo.forms

import scalafx.scene.canvas.{GraphicsContext => SfxGraphicsContext}
import scalafx.geometry.Rectangle2D
import javafx.scene.paint.Color

trait Widget[+T] { self: T =>
  def intersects(formSize: (Double, Double), newRect: Rectangle2D): Boolean = drawArea(formSize).intersects(newRect)
  def draw(formSize: (Double, Double), ctx: SfxGraphicsContext, isSelected: Boolean)
  def drawArea(formSize: (Double, Double)): Rectangle2D
  def contains(formSize: (Double, Double), x: Double, y: Double): Boolean = drawArea(formSize).contains(x, y)
  def possibleMouseOperation(formSize: (Double, Double), x: Double, y: Double): MouseOperation
}

object SelectionWidget {
  val LineWidth = 2.0
}

case class SelectionWidget(
  rect: Rectangle2D
) extends Widget[SelectionWidget] {
  import SelectionWidget._

  def drawArea(formSize: (Double, Double)) = new Rectangle2D(
    rect.minX - LineWidth / 2, rect.minY - LineWidth / 2,
    rect.width + LineWidth, rect.height + LineWidth
  )

  def draw(formSize: (Double, Double), gc: SfxGraphicsContext, isSelected: Boolean) {
    gc.setLineWidth(2.0)
    gc.setLineDashes()
    gc.setStroke(Color.BLACK)
    gc.setLineDashes(5.0, 5.0)
    gc.strokeRect(rect.minX, rect.minY, rect.width, rect.height)
  }

  def possibleMouseOperation(formSize: (Double, Double), x: Double, y: Double): MouseOperation = CanDoNothing
}
