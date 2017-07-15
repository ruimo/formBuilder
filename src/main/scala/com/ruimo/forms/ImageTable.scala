package com.ruimo.forms

import scala.collection.JavaConverters._
import scala.collection.{immutable => imm, mutable => mut}
import java.io.File

trait ImageTableListener {
  def onFilesChanged(before: imm.Set[File], after: imm.Set[File]) {}
}

class ImageTable {
  private[this] val files: mut.SortedSet[File] = mut.TreeSet()
  private[this] val listeners: mut.Set[ImageTableListener] = mut.Set()

  def addFiles(files: java.util.List[File]) {
    addFiles(files.asScala)
  }

  def addFiles(files: Iterable[File]) {
    val before = this.files.toSet
    this.files ++= files
    val after = this.files.toSet
    onFilesChanged(before, after)
  }

  def addChangeListener(l: ImageTableListener) {
    this.listeners += l
  }

  private[this] def onFilesChanged(before: imm.Set[File], after: imm.Set[File]) {
    listeners.foreach { _.onFilesChanged(before, after) }
  }
}
