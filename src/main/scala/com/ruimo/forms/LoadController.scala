package com.ruimo.forms

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

class LoadController extends SaveLoadController {
  @FXML
  protected var configList: TableView[FormConfig] = _

  @FXML
  def onFormTableClicked(e: MouseEvent) {
    val conf: FormConfig = configTable.selectionModel().getSelectedItem
    println("onFormTableClicked " + conf)
    e.getButton match {
      case MouseButton.PRIMARY =>
      case MouseButton.SECONDARY =>
        println("Right clicked")
        showContextMenu(conf, configTable, e)
      case _ =>
    }
  }
}
