package com.ruimo.forms

import com.ruimo.scoins.Percent
import scalafx.scene.control.{TextField => SfxTextField}

sealed trait PercentFieldFailure

object PercentFieldResult {
  case object NotNumber extends PercentFieldFailure
  case class TooSmall(min: Percent) extends PercentFieldFailure
  case class TooBig(max: Percent) extends PercentFieldFailure
  case object Empty extends PercentFieldFailure

  def parseTextField(
    txt: SfxTextField, min: Option[Percent] = None, max: Option[Percent] = None
  ): Either[PercentFieldFailure, Percent] = parseString(txt.text.value, min, max)

  def parseString(
    s: String, min: Option[Percent] = None, max: Option[Percent] = None
  ): Either[PercentFieldFailure, Percent] = s.trim match {
    case "" => Left(Empty)
    case str: String =>
      try {
        val p = Percent(str.toDouble)
        min match {
          case None =>
            max match {
              case None => Right(p)
              case Some(maxPercent) => if (maxPercent < p) Left(TooBig(maxPercent)) else Right(p)
            }
          case Some(minPercent) => if (p < minPercent) Left(TooSmall(minPercent)) else Right(p)
        }
      } catch {
        case e: NumberFormatException =>
          e.printStackTrace()
          Left(NotNumber)
      }
  }
}
