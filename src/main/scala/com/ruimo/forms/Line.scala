package com.ruimo.forms

import scala.math.{Pi, cos, sin, abs}

// Line:
// y = -(cosθ/sinθ)・x + ρ/sinθ For nearly horizontal
// x = -(sinθ/cosθ)・y + ρ/cosθ For nearly vertical
trait Line {
  def y(x: Double): Double
  def x(y: Double): Double
}

case class NearlyHorizontalLine(ro: Double, th: Double) extends Line {
  val a = -cos(th) / sin(th)
  val b = ro / sin(th)

  def y(x: Double): Double = a * x + b
  def x(y: Double): Double = (y - ro / sin(th)) / a
}

case class NearlyVerticalLine(ro: Double, th: Double) extends Line {
  val a = -sin(th) / cos(th)
  val b = ro / cos(th)

  def y(x: Double): Double = (x - ro / cos(th)) / a
  def x(y: Double): Double = a * y + b
}

object Line {
  def apply(ro: Double, th: Double): Line =
    if (Pi / 2 - 0.1 < th && th < Pi / 2 + 0.1) NearlyHorizontalLine(ro, th)
    else NearlyVerticalLine(ro, th)
}
