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

  private val value: Os = {
    val osName = System.getProperty("os.name").toLowerCase
    if (osName.startsWith("linux")) Linux
    else if (osName.startsWith("mac")) Osx
    else if (osName.startsWith("windows")) Windows
    else Other
  }

  def apply(): Os = value
}
