package com.ruimo.forms

import org.slf4j.LoggerFactory

trait HasLogger {
  val logger = LoggerFactory.getLogger(getClass)
}
