package com.ruimo.forms

import java.nio.file.Files
import java.util

import com.ruimo.scoins.PathUtil
import com.typesafe.config.ConfigFactory
import org.specs2.mutable._

class MainControllerSpec extends Specification {
  "Main controller" should {
    "isValidFieldName can detect invalid name." in {
      val mc = new MainController
      mc.isValidFieldName("") === false
      mc.isValidFieldName("123456789012345678901234") === true
      mc.isValidFieldName("1234567890123456789012345") === false
      mc.isValidFieldName("1234ABC_-401234567890123") === true
      mc.isValidFieldName("1234ABC_-40123456#890123") === false
    }
  }
}
