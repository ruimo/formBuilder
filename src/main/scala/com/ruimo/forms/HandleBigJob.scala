package com.ruimo.forms

import javafx.scene.control.ButtonType
import org.slf4j.LoggerFactory

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scalafx.application.Platform
import scalafx.scene.control.Alert
import scalafx.scene.control.Alert.AlertType

trait HandleBigJob extends HasLogger {
  def doBigJob[T](in: Either[T, Future[T]])(f: T => Unit) {
    doBigJob(in, onError = t => showGeneralError())(f)
  }

  def doBigJob[T](in: Either[T, Future[T]], onError: Throwable => Unit = t => showGeneralError())(f: T => Unit) {
    in match {
      case Left(value) => try {
        f(value)
      } catch {
        case t: Throwable =>
          logger.error("Unknown error while performing big job." , t)
          showOtherlError()
      }

      case Right(future) =>
        val dlg = new Alert(AlertType.None)
        dlg.setTitle("サーバ通信中")
        dlg.setContentText("サーバと通信しています")
        dlg.show()

        future.map { ret =>
          Platform.runLater(() -> {
            try {
              f(ret)
            } catch {
              case t: Throwable =>
                logger.error("Unknown error.", t)
                showGeneralError()
            }
            // Avoid JavaFX bug to close dialog.
            dlg.getDialogPane().getButtonTypes().addAll(ButtonType.CANCEL);
            dlg.close()
          })
        }.failed.foreach { t =>
          logger.error("Unknown error while performing big job." , t)
          Platform.runLater(() -> {
            // Avoid JavaFX bug to close dialog.
            dlg.getDialogPane().getButtonTypes().addAll(ButtonType.CANCEL);
            dlg.close()
            onError(t)
          })
        }
    }
  }

  def showOtherlError() {
    val err = new Alert(AlertType.Error)
    err.setTitle("一般エラー")
    err.setContentText("処理に失敗しました。")
    err.show()
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

  def sizeError() {
    val err = new Alert(AlertType.Error)
    err.setTitle("画像サイズエラー")
    err.setContentText("画像のサイズが大きすぎます。")
    err.show()
  }
}
