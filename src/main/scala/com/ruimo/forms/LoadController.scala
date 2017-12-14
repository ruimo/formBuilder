package com.ruimo.forms

import java.net.URL
import java.util.ResourceBundle
import javafx.fxml.{FXML, Initializable}
import javafx.scene.control.TableView

import scalafx.scene.control.{TableView => SfxTableView}

class LoadController extends Initializable with HandleBigJob {
  @FXML
  private[this] var configList: TableView[FormConfig] = _

  private[this] lazy val configTable: SfxTableView[FormConfig] = new SfxTableView(configList)

  override def initialize(url: URL, resourceBundle: ResourceBundle): Unit = {
  }
}
