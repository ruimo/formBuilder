package com.ruimo.forms

import scalafx.scene.control.{Alert => SfxAlert}
import scalafx.scene.control.Alert.AlertType
import scalafx.scene.control.{ButtonType => SfxButtonType}
import scalafx.Includes._
import javafx.scene.input.{KeyCode, KeyEvent, MouseButton, MouseEvent}
import javafx.fxml.{FXML, Initializable}
import java.net.URL
import java.util.ResourceBundle
import javafx.beans.property.ReadOnlyStringWrapper
import javafx.beans.value.ObservableValue
import javafx.scene.control.TableColumn.CellDataFeatures
import javafx.scene.control._
import javafx.util.Callback

import scala.collection.{immutable => imm}
import scalafx.collections.ObservableBuffer
import scalafx.event.ActionEvent
import scalafx.scene.control.{ComboBox => SfxComboBox}
import scalafx.scene.control.{TableView => SfxTableView}
import scalafx.scene.control.{ContextMenu => SfxContextMenu}
import scalafx.scene.control.{MenuItem => SfxMenuItem}

class SaveController extends SaveOpenController {
  @FXML
  private[this] var saveConfigNameText: TextField = _

  @FXML
  protected var configList: TableView[FormConfig] = _

  def configName_=(newName: String) {
    saveConfigNameText.setText(newName)
  }

  def configName: String = saveConfigNameText.getText

  @FXML
  def onFormTableClicked(e: MouseEvent) {
    val conf: FormConfig = configTable.selectionModel().getSelectedItem
    println("onFormTableClicked " + conf)
    e.getButton match {
      case MouseButton.PRIMARY =>
        configName = conf.configName
      case MouseButton.SECONDARY =>
        println("Right clicked")
        showContextMenu(conf, configTable, e)
      case _ =>
    }
  }
}
