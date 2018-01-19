package com.ruimo.forms

import scala.collection.JavaConverters._
import scalafx.collections.ObservableBuffer
import javafx.scene.control._
import javafx.scene.control.TableColumn.CellDataFeatures
import java.net.URL
import java.util.ResourceBundle
import javafx.fxml.{FXML, Initializable}
import javafx.scene.control.TableView
import javafx.util.Callback
import javafx.beans.value.ObservableValue
import javafx.beans.property.ReadOnlyStringWrapper
import javafx.scene.input.{KeyCode, KeyEvent, MouseButton, MouseEvent}

import scalafx.scene.control.{TableView => SfxTableView}

class OpenController extends SaveOpenController {
  @FXML
  protected var configList: TableView[FormConfig] = _

  private[this] var dlg: Alert = _

  def dialog_=(dlg: Alert): Unit = {
    this.dlg = dlg
  }

  def dialog = dlg

  @FXML
  def onFormTableClicked(e: MouseEvent) {
    val conf: FormConfig = configTable.selectionModel().getSelectedItem
    println("onFormTableClicked " + conf)
    e.getButton match {
      case MouseButton.PRIMARY =>
        if (e.getClickCount == 2) {
          dlg.getDialogPane().getButtonTypes().asScala.foreach { bt =>
            if (bt.getButtonData == ButtonBar.ButtonData.APPLY) {
              dlg.getDialogPane().lookupButton(bt).asInstanceOf[Button].fire()
            }
          }
        }
      case MouseButton.SECONDARY =>
        println("Right clicked")
        showContextMenu(conf, configTable, e)
      case _ =>
    }
  }

  def getSelectedConfig: Option[FormConfig] = Option(configTable.selectionModel().getSelectedItem)
}
