package com.ruimo.forms

import scalafx.Includes._
import scalafx.scene.control.{ContextMenu => SfxContextMenu}
import scalafx.scene.control.{MenuItem => SfxMenuItem}
import scalafx.event.ActionEvent
import scalafx.scene.control.{Alert => SfxAlert}
import scalafx.scene.control.Alert.AlertType
import scalafx.scene.control.{ButtonType => SfxButtonType}
import javafx.fxml.Initializable
import java.net.URL
import java.util.ResourceBundle
import javafx.scene.control._
import javafx.scene.control.TableColumn.CellDataFeatures
import scalafx.collections.ObservableBuffer
import javafx.beans.value.ObservableValue
import javafx.beans.property.ReadOnlyStringWrapper
import javafx.util.Callback
import javafx.beans.value.ObservableValue
import javafx.beans.property.ReadOnlyStringWrapper
import scalafx.scene.control.{TableView => SfxTableView}
import javafx.scene.input.{KeyCode, KeyEvent, MouseButton, MouseEvent}

trait SaveOpenController extends Initializable with HandleBigJob {
  protected lazy val configTable: SfxTableView[FormConfig] = new SfxTableView(configList)

  protected def configList: TableView[FormConfig]

  def formConfigs_=(list: Seq[FormConfig]) {
    configList.setItems(ObservableBuffer(list))
  }

  private[this] var _project: Project = _

  def project_=(newProject: Project) {
    _project = newProject
  }

  def project: Project = _project

  override def initialize(url: URL, resourceBundle: ResourceBundle): Unit = {
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
