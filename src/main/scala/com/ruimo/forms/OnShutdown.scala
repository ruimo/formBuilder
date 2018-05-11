package com.ruimo.forms

import java.nio.file.Path

import com.ruimo.scoins.PathUtil
import org.slf4j.LoggerFactory

import scala.collection.{immutable => imm}

object OnShutdown extends HasLogger {
  @volatile
  private[this] var _deleteDirectories = imm.Seq[Path]()

  Runtime.getRuntime().addShutdownHook(new Thread {
    override def run() {
      logger.info("Performing shutdown hook...")
      deleteDirectory()
      logger.info("Shutdown hook done.")
    }
  })

  private[this] def deleteDirectory() {
    _deleteDirectories.foreach { dir =>
      try {
        PathUtil.deleteDir(dir)
      }
      catch {
        case t: Throwable =>
          logger.info("Cannot delete directory " + dir + ". Skip.", t)
      }
    }
  }

  def addDeleteDirectory(dir: Path) {
    _deleteDirectories = _deleteDirectories :+ dir
  }
}
