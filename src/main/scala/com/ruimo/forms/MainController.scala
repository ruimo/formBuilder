package com.ruimo.forms

import scala.collection.{immutable => imm, mutable => mut}

import javafx.collections.FXCollections
import javafx.collections.ObservableList
import javafx.fxml.Initializable
import javafx.fxml.FXML
import javafx.event.ActionEvent
import javafx.scene.input.MouseEvent
import javafx.scene.control.ListView
import javafx.scene.image.ImageView

import scalafx.collections.ObservableBuffer
import scalafx.application.Platform
import scalafx.collections.ObservableSet
import scalafx.scene.image.Image
import scalafx.stage.FileChooser
import scalafx.stage.Stage

import java.io.File
import java.net.URL
import java.util.ArrayList
import java.util.List
import java.util.ResourceBundle
import java.util.Set
import java.util.function.Function
import java.util.stream.Collectors
import scalafx.scene.control.{ListView => SfxListView}
import javafx.scene.canvas.Canvas
import scalafx.scene.canvas.{Canvas => SfxCanvas}

class MainController extends Initializable {
  private var imageTable: ImageTable = new ImageTable
  private var stage: Stage = _

  def setStage(stage: Stage) {
    this.stage = stage
    stage.setWidth(1024)
    stage.setHeight(800)
  }

  @FXML
  private[this] var imageListView: ListView[_] = _

  lazy val sfxImageListView = new SfxListView(imageListView.asInstanceOf[ListView[File]])

  @FXML
  private[this] var imageCanvas: Canvas = _

  lazy val sfxImageCanvas = new SfxCanvas(imageCanvas)

  @FXML
  def exitMenuClicked(event: ActionEvent) {
    println("exitMenuClicked()")
    Platform.exit()
  }

  @FXML
  def imageOpenMenuClicked(event: ActionEvent) {
    val fc = new FileChooser {
      title = "Select image file"
      extensionFilters.add(new FileChooser.ExtensionFilter("Image Files", "*.png"))
    }
    Option(fc.showOpenMultipleDialog(stage)).foreach { files =>
      imageTable.addFiles(files)
    }
  }

  @FXML
  def imageSelected(e: MouseEvent) {
    val file: File = e.getSource().asInstanceOf[ListView[File]].getSelectionModel().getSelectedItem()
    println("Image selected " + file)
    val img: Image = new Image(file.toURI().toString())
    sfxImageCanvas.width = img.width.toDouble
    sfxImageCanvas.height = img.height.toDouble
    val ctx = sfxImageCanvas.graphicsContext2D
    ctx.clearRect(0, 0, sfxImageCanvas.width.toDouble, sfxImageCanvas.height.toDouble)
    ctx.drawImage(img, 0, 0)
  }

  override def initialize(url: URL, resourceBundle: ResourceBundle) {
    imageTable.addChangeListener(new ImageTableListener() {
      override def onFilesChanged(before: imm.Set[File], after: imm.Set[File]) {
        sfxImageListView.items = ObservableBuffer(after.toSeq)
      }
    });
  }
}
