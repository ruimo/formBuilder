package com.ruimo.forms

sealed trait Version {
  def asString: String
}

case object VersionOne extends Version {
  override def asString = "1"
}

