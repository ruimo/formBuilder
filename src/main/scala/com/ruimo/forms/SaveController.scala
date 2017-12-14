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

class SaveController extends Initializable with HandleBigJob {
  @FXML
  private[this] var saveConfigNameText: TextField = _

  @FXML
  private[this] var configList: TableView[FormConfig] = _

  private[this] lazy val configTable: SfxTableView[FormConfig] = new SfxTableView(configList)

  private[this] var _project: Project = _

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

  def project_=(newProject: Project) {
    _project = newProject
  }

  def project: Project = _project

  def formConfigs_=(list: Seq[FormConfig]) {
    configList.setItems(ObservableBuffer(list))
  }

  @FXML
  def onSaveFormClicked(e: MouseEvent) {
    val conf: FormConfig = configTable.selectionModel().getSelectedItem
    println("onSaveFormClicked " + conf)
    e.getButton match {
      case MouseButton.PRIMARY =>
        configName = conf.configName
      case MouseButton.SECONDARY =>
        println("Right clicked")
        showContextMenu(conf, configTable, e)
      case _ =>
    }
  }

  def showContextMenu(conf: FormConfig, t: SfxTableView[FormConfig], e: MouseEvent) {
    val cm = new SfxContextMenu(
      new SfxMenuItem("削除(_D)") {
        mnemonicParsing = true
        onAction = (e: ActionEvent) => {
          println("削除 clicked")
          val dlg = new SfxAlert(AlertType.Confirmation) {
            title = "削除確認"
            contentText = conf.configName + "を削除してよろしいですか？"
            buttonTypes = Seq(
              SfxButtonType.Cancel, SfxButtonType.Yes
            )
          }
          dlg.showAndWait() match {
            case Some(SfxButtonType.Yes) =>
              doBigJob {
                Right(project.removeConfig(conf.configName))
              } {
                case RemoveConfigResultOk(resp) =>
                  doBigJob {
                    Right(project.listConfig())
                  } {
                    case ListConfigResultOk(resp) =>
                      formConfigs_=(resp.configTable)
                    case authFail: RestAuthFailure =>
                      authError()
                    case serverFail: RestUnknownFailure =>
                      showGeneralError()
                  }
                case authFail: RestAuthFailure =>
                  authError()
                case serverFail: RestUnknownFailure =>
                  showGeneralError()
              }
            case _ =>
              e.consume()
          }
        }
      }
    )
    cm.show(t, e.getScreenX, e.getScreenY)
  }
}
