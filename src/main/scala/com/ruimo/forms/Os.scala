package com.ruimo.forms

import com.ruimo.scoins.Version

sealed trait Os {
  def fileName(v: Version): String
}

object Os {
  case object Linux extends Os {
    override def fileName(v: Version): String = "formbuilder_linux-" + v + ".zip"
  }

  case object Osx extends Os {
    override def fileName(v: Version): String = "formbuilder_osx-" + v + ".zip"
  }

  case object Windows extends Os {
    override def fileName(v: Version): String = "formbuilder_win-" + v + ".zip"
  }

  case object Other extends Os {
    override def fileName(v: Version): String = "formbuilder-" + v + ".tgz"
  }

  private val value: Os = System.getProperty("os.name").toLowerCase match {
    case "linux" => Linux
    case "mac" => Osx
    case "windows" => Windows
    case _ => Other
  }

  def apply(): Os = value
}
