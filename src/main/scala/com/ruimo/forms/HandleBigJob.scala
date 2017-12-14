package com.ruimo.forms

import javafx.scene.control.ButtonType

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scalafx.application.Platform
import scalafx.scene.control.Alert
import scalafx.scene.control.Alert.AlertType

trait HandleBigJob {
  def doBigJob[T](in: Either[T, Future[T]])(f: T => Unit) {
    doBigJob(in, onError = t => showGeneralError())(f)
  }

  def doBigJob[T](in: Either[T, Future[T]], onError: Throwable => Unit = t => showGeneralError())(f: T => Unit) {
    in match {
      case Left(value) => f(value)
      case Right(future) =>
        val dlg = new Alert(AlertType.None)
        dlg.setTitle("サーバ通信中")
        dlg.setContentText("サーバと通信しています")
        dlg.show()

        future.map { ret =>
          Platform.runLater(() -> {
            f(ret)
            // Avoid JavaFX bug to close dialog.
            dlg.getDialogPane().getButtonTypes().addAll(ButtonType.CANCEL);
            dlg.close()
          })
        }.failed.foreach { t =>
          t.printStackTrace
          Platform.runLater(() -> {
            // Avoid JavaFX bug to close dialog.
            dlg.getDialogPane().getButtonTypes().addAll(ButtonType.CANCEL);
            dlg.close()
            onError(t)
          })
        }
    }
  }

  def showGeneralError() {
    val err = new Alert(AlertType.Error)
    err.setTitle("サーバ・エラー")
    err.setContentText("サーバ処理に失敗しました。")
    err.show()
  }

  def authError() {
    val err = new Alert(AlertType.Error)
    err.setTitle("認証エラー")
    err.setContentText("サーバでの認証に失敗しました。設定を確認してください。")
    err.show()
  }
}
