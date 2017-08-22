package com.ruimo.forms

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
