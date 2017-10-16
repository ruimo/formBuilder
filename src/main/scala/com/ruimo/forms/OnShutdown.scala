package com.ruimo.forms

import java.nio.file.Path

import com.ruimo.scoins.PathUtil

import scala.collection.{immutable => imm}

object OnShutdown {
  @volatile
  private[this] var _deleteDirectories = imm.Seq[Path]()

  Runtime.getRuntime().addShutdownHook(new Thread {
    override def run() {
      println("Performing shutdown hook...")
      deleteDirectory()
      println("Shutdown hook done.")
    }
  })

  private[this] def deleteDirectory() {
    _deleteDirectories.foreach { dir =>
      try {
        PathUtil.deleteDir(dir)
      }
      catch {
        case t: Throwable =>
          println("Cannot delete directory " + dir + ". Skip.")
          t.printStackTrace()
      }
    }
  }

  def addDeleteDirectory(dir: Path) {
    _deleteDirectories = _deleteDirectories :+ dir
  }
}
