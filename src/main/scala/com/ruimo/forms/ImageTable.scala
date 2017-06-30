package com.ruimo.forms

import scala.collection.JavaConverters._
import scala.collection.{immutable => imm, mutable => mut}
import java.io.File

trait ImageTableListener {
  def onFilesChanged(before: java.util.Set[File], after: java.util.Set[File]) {}
}

class ImageTable {
  private[this] val files: mut.SortedSet[File] = mut.TreeSet()
  private[this] val listeners: mut.Set[ImageTableListener] = mut.Set()

  def addFiles(files: java.util.List[File]) {
    addFiles(files.asScala)
  }

  def addFiles(files: Iterable[File]) {
    val before = new java.util.TreeSet(this.files.asJava)
    this.files ++= files
    val after = new java.util.TreeSet(this.files.asJava)
    onFilesChanged(before, after)
  }

  def addChangeListener(l: ImageTableListener) {
    this.listeners += l
  }

  private[this] def onFilesChanged(before: java.util.Set[File], after: java.util.Set[File]) {
    listeners.foreach { _.onFilesChanged(before, after) }
  }
}
