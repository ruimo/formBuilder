package com.ruimo.forms

import java.awt.Button

import scala.collection.{immutable => imm, mutable => mut}
import javafx.fxml.{FXML, FXMLLoader, Initializable}
import javafx.event.ActionEvent
import javafx.scene.input.MouseEvent
import javafx.event.EventHandler

import scalafx.collections.ObservableBuffer
import scalafx.application.Platform
import scalafx.scene.image.Image
import scalafx.stage.{FileChooser, Modality, Stage, StageStyle}
import java.io.File
import java.net.URL
import java.util.ResourceBundle
import javafx.scene.Scene

import scalafx.scene.control.{Alert => SfxAlert, ListView => SfxListView}
import javafx.scene.canvas.Canvas
import javafx.scene.control._

import scalafx.scene.canvas.{Canvas => SfxCanvas}
import scalafx.scene.control.{DialogPane => SfxDialogPane}
import scalafx.scene.control.Alert.AlertType

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
  private[this] var skewCorrectionCheckBox: CheckBox = _

  @FXML
  private[this] var skewCorrectionDetailButton: Button = _

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

  @FXML
  def skewCorrectionChanged(e: ActionEvent) {
    println("skewCorrectionChanged")
  }

  @FXML
  def skewCorrectionDetailClicked(e: ActionEvent) {
    println("skewCorrectionDetail")
    val loader = new FXMLLoader(getClass().getResource("skewCorrectionDialog.fxml"))
    loader.setController(new SkewCorrectionDetailController())
    val alert = new SfxAlert(AlertType.Confirmation)
    alert.dialogPane = new SfxDialogPane(loader.load())
    alert.title = "傾き補正"
    alert.onCloseRequest = new EventHandler[DialogEvent] {
      override def handle(t: DialogEvent) {
        println("event type = " + t.getEventType)
        println("target = " + t.getTarget)
        println("result = " + t.getTarget.asInstanceOf[Alert].getResult)
        t.consume()
      }
    }
    alert.showAndWait()

//    val scene: Scene = new Scene(root)
//    val myStage = new Stage()
//    myStage.setTitle("傾き補正")
//    myStage.initModality(Modality.ApplicationModal)
//    myStage.initOwner(stage.getOwner())
//    myStage.setScene(scene)
//    myStage.show()
  }

  override def initialize(url: URL, resourceBundle: ResourceBundle) {
    imageTable.addChangeListener(new ImageTableListener() {
      override def onFilesChanged(before: imm.Set[File], after: imm.Set[File]) {
        sfxImageListView.items = ObservableBuffer(after.toSeq)
      }
    });
  }
}
