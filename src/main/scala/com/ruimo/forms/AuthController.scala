package com.ruimo.forms

import javafx.fxml.{FXML, Initializable}
import java.net.URL
import java.util.ResourceBundle
import javafx.scene.control.{ComboBox, TextField}

import scalafx.collections.ObservableBuffer
import scalafx.scene.control.{ComboBox => SfxComboBox}

class AuthController extends Initializable {
  @FXML
  private[this] var serverUrlText: TextField = _

  @FXML
  private[this] var contractedUserIdText: TextField = _

  @FXML
  private[this] var applicationTokenText: TextField = _

  override def initialize(url: URL, resourceBundle: ResourceBundle) {
  }

  def model: AuthSettings = AuthSettingsImpl(
    ContractedUserId(contractedUserIdText.getText()),
    ApplicationToken(applicationTokenText.getText()),
    Url(serverUrlText.getText())
  )

  def model_=(newModel: AuthSettings) {
    newModel match {
      case NullAuthSettings =>
        contractedUserIdText.setText("")
        applicationTokenText.setText("")
        serverUrlText.setText("")

      case as: AuthSettingsImpl =>
        contractedUserIdText.setText(as.contractedUserId.value)
        applicationTokenText.setText(as.applicationToken.value)
        serverUrlText.setText(as.url.value)
    }
  }
}

