package com.ruimo.forms

import javafx.fxml.{FXML, Initializable}
import java.net.URL
import java.util.ResourceBundle
import javafx.scene.control.{ComboBox, TextField}

import scalafx.collections.ObservableBuffer
import scalafx.scene.control.{ComboBox => SfxComboBox}

class SaveController extends Initializable {
  @FXML
  private[this] var cormConfigNameText: TextField = _

  override def initialize(url: URL, resourceBundle: ResourceBundle) {
  }
}
