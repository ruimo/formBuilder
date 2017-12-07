package com.ruimo.forms

import javafx.fxml.{FXML, Initializable}
import java.net.URL
import java.time.Instant
import java.util.ResourceBundle
import javafx.beans.property.ReadOnlyStringWrapper
import javafx.beans.value.ObservableValue
import javafx.scene.control.TableColumn.CellDataFeatures
import javafx.scene.control.{ComboBox, TableColumn, TableView, TextField}
import javafx.util.Callback

import scala.collection.{immutable => imm}
import scalafx.collections.ObservableBuffer
import scalafx.scene.control.{ComboBox => SfxComboBox}
import scalafx.scene.control.{TableView => SfxTableView}

class SaveController extends Initializable {
  @FXML
  private[this] var saveConfigNameText: TextField = _

  @FXML
  private[this] var configList: TableView[FormConfig] = _

  override def initialize(url: URL, resourceBundle: ResourceBundle) {
    val nameCol = configList.getColumns().get(0).asInstanceOf[TableColumn[FormConfig, String]]
    val dateCol = configList.getColumns().get(1).asInstanceOf[TableColumn[FormConfig, String]]
    val revCol = configList.getColumns().get(2).asInstanceOf[TableColumn[FormConfig, String]]
    val commentCol = configList.getColumns().get(3).asInstanceOf[TableColumn[FormConfig, String]]

    nameCol.setCellValueFactory(new Callback[CellDataFeatures[FormConfig, String], ObservableValue[String]]() {
      def call(c: CellDataFeatures[FormConfig, String]): ObservableValue[String] = {
        new ReadOnlyStringWrapper(c.getValue().configName)
      }
    })
    dateCol.setCellValueFactory(new Callback[CellDataFeatures[FormConfig, String], ObservableValue[String]]() {
      def call(c: CellDataFeatures[FormConfig, String]): ObservableValue[String] = {
        new ReadOnlyStringWrapper(c.getValue().createdAt.toString())
      }
    })
    revCol.setCellValueFactory(new Callback[CellDataFeatures[FormConfig, String], ObservableValue[String]]() {
      def call(c: CellDataFeatures[FormConfig, String]): ObservableValue[String] = {
        new ReadOnlyStringWrapper(c.getValue().revision.value.toString)
      }
    })
    commentCol.setCellValueFactory(new Callback[CellDataFeatures[FormConfig, String], ObservableValue[String]]() {
      def call(c: CellDataFeatures[FormConfig, String]): ObservableValue[String] = {
        new ReadOnlyStringWrapper(c.getValue().comment)
      }
    })
//    configList.setItems(ObservableBuffer(FormConfigRow("Hello", Instant.now(), 123L, "comment")))
  }

  def configName_=(newName: String) {
    saveConfigNameText.setText(newName)
  }

  def configName: String = saveConfigNameText.getText

  def formConfigs_=(list: Seq[FormConfig]) {
    configList.setItems(ObservableBuffer(list))
  }
}
