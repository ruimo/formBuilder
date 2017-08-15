package com.ruimo.forms

import scalafx.geometry.{Point2D, Rectangle2D}

object Helpers {
  def toRect(p0: Point2D, p1: Point2D): Rectangle2D = {
    val (x: Double, w: Double) = if (p0.x < p1.x) (p0.x, p1.x - p0.x) else (p1.x, p0.x - p1.x)
    val (y: Double, h: Double) = if (p0.y < p1.y) (p0.y, p1.y - p0.y) else (p1.y, p0.y - p1.y)
    new Rectangle2D(x, y, w, h)
  }
}
