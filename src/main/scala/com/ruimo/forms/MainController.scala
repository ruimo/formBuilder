package com.ruimo.forms

import generated.BuildInfo
import java.util.prefs.Preferences

import scala.math
import scalafx.scene.control.{ButtonType => SfxButtonType}
import scala.annotation.tailrec
import scala.collection.JavaConverters._
import scala.concurrent.{Await, Future}
import java.util.zip.ZipInputStream
import javafx.animation.AnimationTimer

import scala.math.{Pi, abs, cos, sin}
import java.io._

import scalafx.scene.canvas.{GraphicsContext => SfxGraphicsContext}
import scala.collection.{immutable => imm, mutable => mut}
import javafx.fxml.{FXML, FXMLLoader, Initializable}
import javafx.event.ActionEvent
import javafx.scene.input.{KeyCode, KeyEvent, MouseEvent}
import javafx.event.EventHandler

import scalafx.collections.ObservableBuffer
import scalafx.application.Platform
import scalafx.scene.image.Image
import scalafx.stage.{FileChooser, Modality, Stage, StageStyle}
import java.net.{HttpURLConnection, URL}
import java.nio.channels.FileChannel
import java.nio.charset.Charset
import java.nio.file._
import java.util
import java.util.ResourceBundle
import java.util.concurrent.TimeUnit
import java.util.zip.{ZipEntry, ZipOutputStream}
import javafx.scene.{Cursor, Scene}

import scalafx.scene.control.{Alert => SfxAlert, ListView => SfxListView, TextInputDialog => SfxTextInputDialog}
import scalafx.scene.control.{ChoiceDialog => SfxChoiceDialog}
import scalafx.scene.control.{CheckBox => SfxCheckBox}
import scalafx.scene.control.{ButtonType => SfxButtonType}
import javafx.scene.canvas.Canvas
import javafx.scene.control._
import javafx.scene.paint.Color
import javafx.stage.WindowEvent

import scalafx.scene.canvas.{Canvas => SfxCanvas}
import scalafx.scene.control.{DialogPane => SfxDialogPane}
import scalafx.scene.control.Alert.AlertType
import play.api.libs.ws.JsonBodyReadables._
import play.api.libs.ws.JsonBodyWritables._
import play.api.libs.ws.DefaultBodyReadables._
import play.api.libs.ws.DefaultBodyWritables._

import scala.concurrent.ExecutionContext.Implicits.global
import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import com.ruimo.scoins.{LoanPattern, PathUtil, Version, Zip}
import com.ruimo.scoins.LoanPattern._
import play.api.libs.json._

import scalafx.geometry.{Point2D, Rectangle2D}
import Helpers.toRect
import akka.stream.scaladsl.FileIO

import scala.concurrent.duration.Duration
import scala.concurrent.duration._
import scala.util.Try

case class EditorContext(
  drawWidget: (Widget[_], Boolean) => Unit,
  redrawRect: (Rectangle2D) => Unit,
  fieldCreated: (Field) => Unit,
  drawSelectionRect: (Rectangle2D) => Unit,
  selectFields: (Rectangle2D, MouseEvent) => Unit,
  setMouseCursor: Cursor => Unit
)

object ModeEditors {
  sealed trait ModeEditor {
    def onMousePressed(e: MouseEvent, imgSz: (Double, Double)): ModeEditor
    def onMouseDragged(e: MouseEvent, imgSz: (Double, Double)): ModeEditor
    def onMouseReleased(e: MouseEvent, imgSz: (Double, Double)): ModeEditor
    def onMouseEntered(e: MouseEvent, imgSz: (Double, Double)): ModeEditor
    def onMouseExited(e: MouseEvent, imgSz: (Double, Double)): ModeEditor
    def onMouseMoved(e: MouseEvent, imgSz: (Double, Double)): ModeEditor
    def switchToAddMode(e: ActionEvent, imgSz: (Double, Double)): ModeEditor
    def switchToAddCropMode(e: ActionEvent, imgSz: (Double, Double)): ModeEditor
    def switchToSelectMode(e: ActionEvent, imgSz: (Double, Double)): ModeEditor
  }

  object AddCropMode {
    case class Init(
      project: Project,
      editorContext: EditorContext
    ) extends ModeEditor {
      def onMousePressed(e: MouseEvent, imgSz: (Double, Double)): ModeEditor = {
        project.deselectAllFields()
        Start(project, editorContext, new Point2D(e.getX, e.getY))
      }
      def onMouseDragged(e: MouseEvent, imgSz: (Double, Double)): ModeEditor = this
      def onMouseReleased(e: MouseEvent, imgSz: (Double, Double)): ModeEditor = this
      def onMouseEntered(e: MouseEvent, imgSz: (Double, Double)): ModeEditor = this
      def onMouseExited(e: MouseEvent, imgSz: (Double, Double)): ModeEditor = this
      def onMouseMoved(e: MouseEvent, imgSz: (Double, Double)): ModeEditor = this
      def switchToAddMode(e: ActionEvent, imgSz: (Double, Double)): ModeEditor = AddMode.Init(project, editorContext)
      def switchToAddCropMode(e: ActionEvent, imgSz: (Double, Double)): ModeEditor = this
      def switchToSelectMode(e: ActionEvent, imgSz: (Double, Double)): ModeEditor = SelectMode.Init(project, editorContext)
    }

    case class Start(
      project: Project,
      editorContext: EditorContext,
      p0: Point2D
    ) extends ModeEditor {
      def onMousePressed(e: MouseEvent, imgSz: (Double, Double)): ModeEditor = this
      def onMouseDragged(e: MouseEvent, imgSz: (Double, Double)): ModeEditor = {
        val p1 = new Point2D(e.getX, e.getY)
        val adding = UnknownCropFieldImpl(imgSz, toRect(p0, p1))
        editorContext.drawWidget(adding, true)
        Dragging(adding, project, editorContext, p0, p1)
      }
      def onMouseReleased(e: MouseEvent, imgSz: (Double, Double)): ModeEditor = Init(project, editorContext)
      def onMouseEntered(e: MouseEvent, imgSz: (Double, Double)): ModeEditor = this
      def onMouseExited(e: MouseEvent, imgSz: (Double, Double)): ModeEditor = this
      def onMouseMoved(e: MouseEvent, imgSz: (Double, Double)): ModeEditor = this
      def switchToAddMode(e: ActionEvent, imgSz: (Double, Double)): ModeEditor = AddMode.Init(project, editorContext)
      def switchToAddCropMode(e: ActionEvent, imgSz: (Double, Double)): ModeEditor = Init(project, editorContext)
      def switchToSelectMode(e: ActionEvent, imgSz: (Double, Double)): ModeEditor = SelectMode.Init(project, editorContext)
    }

    case class Dragging(
      adding: Field,
      project: Project,
      editorContext: EditorContext,
      p0: Point2D, p1: Point2D
    ) extends ModeEditor {
      def onMousePressed(e: MouseEvent, imgSz: (Double, Double)): ModeEditor = this
      def onMouseDragged(e: MouseEvent, imgSz: (Double, Double)): ModeEditor = {
        val newP1 = new Point2D(e.getX, e.getY)
        editorContext.redrawRect(adding.drawArea(imgSz))
        val newAdding = UnknownCropFieldImpl(imgSz, toRect(p0, newP1))
        editorContext.drawWidget(newAdding, true)
        Dragging(newAdding, project, editorContext, p0, newP1)
      }
      def onMouseReleased(e: MouseEvent, imgSz: (Double, Double)): ModeEditor = {
        val newP1 = new Point2D(e.getX, e.getY)
        editorContext.redrawRect(adding.drawArea(imgSz))
        val newAdding = UnknownCropFieldImpl(imgSz, toRect(p0, newP1))
        editorContext.drawWidget(newAdding, true)
        editorContext.fieldCreated(newAdding)
        Init(project, editorContext)
      }
      def onMouseEntered(e: MouseEvent, imgSz: (Double, Double)): ModeEditor = this
      def onMouseExited(e: MouseEvent, imgSz: (Double, Double)): ModeEditor = this
      def onMouseMoved(e: MouseEvent, imgSz: (Double, Double)): ModeEditor = this
      def switchToAddMode(e: ActionEvent, imgSz: (Double, Double)): ModeEditor = AddMode.Init(project, editorContext)
      def switchToAddCropMode(e: ActionEvent, imgSz: (Double, Double)): ModeEditor = Init(project, editorContext)
      def switchToSelectMode(e: ActionEvent, imgSz: (Double, Double)): ModeEditor = SelectMode.Init(project, editorContext)
    }
  }

  object AddMode {
    case class Init(
      project: Project,
      editorContext: EditorContext
    ) extends ModeEditor {
      assume(project != null)
      assume(editorContext != null)

      def onMousePressed(e: MouseEvent, imgSz: (Double, Double)): ModeEditor = {
        project.deselectAllFields()
        Start(project, editorContext, new Point2D(e.getX, e.getY))
      }
      def onMouseDragged(e: MouseEvent, imgSz: (Double, Double)): ModeEditor = this
      def onMouseReleased(e: MouseEvent, imgSz: (Double, Double)): ModeEditor = this
      def onMouseEntered(e: MouseEvent, imgSz: (Double, Double)): ModeEditor = this
      def onMouseExited(e: MouseEvent, imgSz: (Double, Double)): ModeEditor = this
      def onMouseMoved(e: MouseEvent, imgSz: (Double, Double)): ModeEditor = this
      def switchToAddMode(e: ActionEvent, imgSz: (Double, Double)): ModeEditor = this
      def switchToAddCropMode(e: ActionEvent, imgSz: (Double, Double)): ModeEditor = AddCropMode.Init(project, editorContext)
      def switchToSelectMode(e: ActionEvent, imgSz: (Double, Double)): ModeEditor = SelectMode.Init(project, editorContext)
    }

    case class Start(
      project: Project,
      editorContext: EditorContext,
      p0: Point2D
    ) extends ModeEditor {
      def onMousePressed(e: MouseEvent, imgSz: (Double, Double)): ModeEditor = this
      def onMouseDragged(e: MouseEvent, imgSz: (Double, Double)): ModeEditor = {
        val p1 = new Point2D(e.getX, e.getY)
        val adding = AbsoluteFieldImpl(imgSz, toRect(p0, p1), "adding")
        editorContext.drawWidget(adding, true)
        Dragging(adding, project, editorContext, p0, p1)
      }
      def onMouseReleased(e: MouseEvent, imgSz: (Double, Double)): ModeEditor = Init(project, editorContext)
      def onMouseEntered(e: MouseEvent, imgSz: (Double, Double)): ModeEditor = this
      def onMouseExited(e: MouseEvent, imgSz: (Double, Double)): ModeEditor = this
      def onMouseMoved(e: MouseEvent, imgSz: (Double, Double)): ModeEditor = this
      def switchToAddMode(e: ActionEvent, imgSz: (Double, Double)): ModeEditor = AddMode.Init(project, editorContext)
      def switchToAddCropMode(e: ActionEvent, imgSz: (Double, Double)): ModeEditor = AddCropMode.Init(project, editorContext)
      def switchToSelectMode(e: ActionEvent, imgSz: (Double, Double)): ModeEditor = SelectMode.Init(project, editorContext)
    }

    case class Dragging(
      adding: Field,
      project: Project,
      editorContext: EditorContext,
      p0: Point2D, p1: Point2D
    ) extends ModeEditor {
      def onMousePressed(e: MouseEvent, imgSz: (Double, Double)): ModeEditor = this
      def onMouseDragged(e: MouseEvent, imgSz: (Double, Double)): ModeEditor = {
        val newP1 = new Point2D(e.getX, e.getY)
        editorContext.redrawRect(adding.drawArea(imgSz))
        val newAdding = AbsoluteFieldImpl(imgSz, toRect(p0, newP1), "adding")
        editorContext.drawWidget(newAdding, true)
        Dragging(newAdding, project, editorContext, p0, newP1)
      }
      def onMouseReleased(e: MouseEvent, imgSz: (Double, Double)): ModeEditor = {
        val newP1 = new Point2D(e.getX, e.getY)
        editorContext.redrawRect(adding.drawArea(imgSz))
        val newAdding = AbsoluteFieldImpl(imgSz, toRect(p0, newP1), "adding")
        editorContext.drawWidget(newAdding, true)
        editorContext.fieldCreated(newAdding)
        Init(project, editorContext)
      }
      def onMouseEntered(e: MouseEvent, imgSz: (Double, Double)): ModeEditor = this
      def onMouseExited(e: MouseEvent, imgSz: (Double, Double)): ModeEditor = this
      def onMouseMoved(e: MouseEvent, imgSz: (Double, Double)): ModeEditor = this
      def switchToAddMode(e: ActionEvent, imgSz: (Double, Double)): ModeEditor = AddMode.Init(project, editorContext)
      def switchToAddCropMode(e: ActionEvent, imgSz: (Double, Double)): ModeEditor = AddCropMode.Init(project, editorContext)
      def switchToSelectMode(e: ActionEvent, imgSz: (Double, Double)): ModeEditor = SelectMode.Init(project, editorContext)
    }
  }

  object SelectMode {
    case class Init(
      project: Project,
      editorContext: EditorContext
    ) extends ModeEditor {
      def onMousePressed(e: MouseEvent, imgSz: (Double, Double)): ModeEditor = {
        def doNext(f: Field): ModeEditor = {
          f.possibleMouseOperation(imgSz, e.getX, e.getY) match {
            case CanMove(_) => Moving(project, editorContext, new Point2D(e.getX, e.getY))
            case CanNorthResize(f) => NorthResize(project, editorContext, new Point2D(e.getX, e.getY))
            case CanEastResize(_) => EastResize(project, editorContext, new Point2D(e.getX, e.getY))
            case CanWestResize(_) => WestResize(project, editorContext, new Point2D(e.getX, e.getY))
            case CanSouthResize(_) => SouthResize(project, editorContext, new Point2D(e.getX, e.getY))
            case CanNorthWestResize(_) => NorthWestResize(project, editorContext, new Point2D(e.getX, e.getY))
            case CanNorthEastResize(_) => NorthEastResize(project, editorContext, new Point2D(e.getX, e.getY))
            case CanSouthWestResize(_) => SouthWestResize(project, editorContext, new Point2D(e.getX, e.getY))
            case CanSouthEastResize(_) => SouthEastResize(project, editorContext, new Point2D(e.getX, e.getY))
            case CanDoNothing => Moving(project, editorContext, new Point2D(e.getX, e.getY))
          }
        }

        project.getSelectedFieldAt(imgSz, e.getX, e.getY) match {
          case Some(f) => doNext(f)
          case None =>
            project.getNormalFieldAt(imgSz, e.getX, e.getY) match {
              case Some(f) =>
                if (! e.isShiftDown)
                  project.deselectAllFields()
                project.selectField(f)
                doNext(f)
              case None =>
                if (! e.isShiftDown)
                  project.deselectAllFields()
                Start(project, editorContext, new Point2D(e.getX, e.getY))
            }
        }
      }
      def onMouseDragged(e: MouseEvent, imgSz: (Double, Double)): ModeEditor = this
      def onMouseReleased(e: MouseEvent, imgSz: (Double, Double)): ModeEditor = this
      def onMouseEntered(e: MouseEvent, imgSz: (Double, Double)): ModeEditor = onMouseMoved(e, imgSz)
      def onMouseExited(e: MouseEvent, imgSz: (Double, Double)): ModeEditor = {
        editorContext.setMouseCursor(Cursor.DEFAULT)
        this
      }
      def onMouseMoved(e: MouseEvent, imgSz: (Double, Double)): ModeEditor = {
        project.possibleMouseOperation(imgSz, e.getX, e.getY) match {
          case CanDoNothing =>
            editorContext.setMouseCursor(Cursor.DEFAULT)
          case CanMove(f) =>
            editorContext.setMouseCursor(Cursor.MOVE)
          case CanNorthResize(f) =>
            editorContext.setMouseCursor(Cursor.N_RESIZE)
          case CanEastResize(f) =>
            editorContext.setMouseCursor(Cursor.E_RESIZE)
          case CanWestResize(f) =>
            editorContext.setMouseCursor(Cursor.W_RESIZE)
          case CanSouthResize(f) =>
            editorContext.setMouseCursor(Cursor.S_RESIZE)
          case CanNorthWestResize(f) =>
            editorContext.setMouseCursor(Cursor.NW_RESIZE)
          case CanNorthEastResize(f) =>
            editorContext.setMouseCursor(Cursor.NE_RESIZE)
          case CanSouthWestResize(f) =>
            editorContext.setMouseCursor(Cursor.SW_RESIZE)
          case CanSouthEastResize(f) =>
            editorContext.setMouseCursor(Cursor.SE_RESIZE)
        }

        this
      }
      def switchToAddMode(e: ActionEvent, imgSz: (Double, Double)): ModeEditor = AddMode.Init(project, editorContext)
      def switchToAddCropMode(e: ActionEvent, imgSz: (Double, Double)): ModeEditor = AddCropMode.Init(project, editorContext)
      def switchToSelectMode(e: ActionEvent, imgSz: (Double, Double)): ModeEditor = this
    }

    case class Start(
      project: Project,
      editorContext: EditorContext,
      p0: Point2D
    ) extends ModeEditor {
      def onMousePressed(e: MouseEvent, imgSz: (Double, Double)): ModeEditor = this
      def onMouseDragged(e: MouseEvent, imgSz: (Double, Double)): ModeEditor = {
        val p1 = new Point2D(e.getX, e.getY)
        val sw = SelectionWidget(toRect(p0, p1))
        editorContext.drawWidget(sw, false)
        Dragging(sw, project, editorContext, p0, p1)
      }
      def onMouseReleased(e: MouseEvent, imgSz: (Double, Double)): ModeEditor = {
        project.selectSingleFieldAt(imgSz, e.getX, e.getY)
        editorContext.setMouseCursor(Cursor.DEFAULT)
        Init(project, editorContext)
      }
      def onMouseEntered(e: MouseEvent, imgSz: (Double, Double)): ModeEditor = this
      def onMouseExited(e: MouseEvent, imgSz: (Double, Double)): ModeEditor = {
        editorContext.setMouseCursor(Cursor.DEFAULT)
        this
      }
      def onMouseMoved(e: MouseEvent, imgSz: (Double, Double)): ModeEditor = this
      def switchToAddMode(e: ActionEvent, imgSz: (Double, Double)): ModeEditor = AddMode.Init(project, editorContext)
      def switchToAddCropMode(e: ActionEvent, imgSz: (Double, Double)): ModeEditor = AddCropMode.Init(project, editorContext)
      def switchToSelectMode(e: ActionEvent, imgSz: (Double, Double)): ModeEditor = SelectMode.Init(project, editorContext)
    }

    case class Moving(
      project: Project,
      editorContext: EditorContext,
      p0: Point2D
    ) extends ModeEditor {
      def onMousePressed(e: MouseEvent, imgSz: (Double, Double)): ModeEditor = this
      def onMouseDragged(e: MouseEvent, imgSz: (Double, Double)): ModeEditor = {
        val p1 = new Point2D(e.getX, e.getY)
        project.moveSelectedFields(imgSz, p0, p1)
        Moving(project, editorContext, p1)
      }
      def onMouseReleased(e: MouseEvent, imgSz: (Double, Double)): ModeEditor = {
        editorContext.setMouseCursor(Cursor.DEFAULT)
        Init(project, editorContext)
      }
      def onMouseEntered(e: MouseEvent, imgSz: (Double, Double)): ModeEditor = this
      def onMouseExited(e: MouseEvent, imgSz: (Double, Double)): ModeEditor = this
      def onMouseMoved(e: MouseEvent, imgSz: (Double, Double)): ModeEditor = this
      def switchToAddMode(e: ActionEvent, imgSz: (Double, Double)): ModeEditor = AddMode.Init(project, editorContext)
      def switchToAddCropMode(e: ActionEvent, imgSz: (Double, Double)): ModeEditor = AddCropMode.Init(project, editorContext)
      def switchToSelectMode(e: ActionEvent, imgSz: (Double, Double)): ModeEditor = SelectMode.Init(project, editorContext)
    }

    case class NorthResize(
      project: Project,
      editorContext: EditorContext,
      p0: Point2D
    ) extends ModeEditor {
      def onMousePressed(e: MouseEvent, imgSz: (Double, Double)): ModeEditor = this
      def onMouseDragged(e: MouseEvent, imgSz: (Double, Double)): ModeEditor = {
        val p1 = new Point2D(e.getX, e.getY)
        project.northResizeSelectedFields(imgSz, p0, p1)
        NorthResize(project, editorContext, p1)
      }
      def onMouseReleased(e: MouseEvent, imgSz: (Double, Double)): ModeEditor = {
        editorContext.setMouseCursor(Cursor.DEFAULT)
        Init(project, editorContext)
      }
      def onMouseEntered(e: MouseEvent, imgSz: (Double, Double)): ModeEditor = this
      def onMouseExited(e: MouseEvent, imgSz: (Double, Double)): ModeEditor = this
      def onMouseMoved(e: MouseEvent, imgSz: (Double, Double)): ModeEditor = this
      def switchToAddMode(e: ActionEvent, imgSz: (Double, Double)): ModeEditor = AddMode.Init(project, editorContext)
      def switchToAddCropMode(e: ActionEvent, imgSz: (Double, Double)): ModeEditor = AddCropMode.Init(project, editorContext)
      def switchToSelectMode(e: ActionEvent, imgSz: (Double, Double)): ModeEditor = SelectMode.Init(project, editorContext)
    }

    case class EastResize(
      project: Project,
      editorContext: EditorContext,
      p0: Point2D
    ) extends ModeEditor {
      def onMousePressed(e: MouseEvent, imgSz: (Double, Double)): ModeEditor = this
      def onMouseDragged(e: MouseEvent, imgSz: (Double, Double)): ModeEditor = {
        val p1 = new Point2D(e.getX, e.getY)
        project.eastResizeSelectedFields(imgSz, p0, p1)
        EastResize(project, editorContext, p1)
      }
      def onMouseReleased(e: MouseEvent, imgSz: (Double, Double)): ModeEditor = {
        editorContext.setMouseCursor(Cursor.DEFAULT)
        Init(project, editorContext)
      }
      def onMouseEntered(e: MouseEvent, imgSz: (Double, Double)): ModeEditor = this
      def onMouseExited(e: MouseEvent, imgSz: (Double, Double)): ModeEditor = this
      def onMouseMoved(e: MouseEvent, imgSz: (Double, Double)): ModeEditor = this
      def switchToAddMode(e: ActionEvent, imgSz: (Double, Double)): ModeEditor = AddMode.Init(project, editorContext)
      def switchToAddCropMode(e: ActionEvent, imgSz: (Double, Double)): ModeEditor = AddCropMode.Init(project, editorContext)
      def switchToSelectMode(e: ActionEvent, imgSz: (Double, Double)): ModeEditor = SelectMode.Init(project, editorContext)
    }

    case class WestResize(
      project: Project,
      editorContext: EditorContext,
      p0: Point2D
    ) extends ModeEditor {
      def onMousePressed(e: MouseEvent, imgSz: (Double, Double)): ModeEditor = this
      def onMouseDragged(e: MouseEvent, imgSz: (Double, Double)): ModeEditor = {
        val p1 = new Point2D(e.getX, e.getY)
        project.westResizeSelectedFields(imgSz, p0, p1)
        WestResize(project, editorContext, p1)
      }
      def onMouseReleased(e: MouseEvent, imgSz: (Double, Double)): ModeEditor = {
        editorContext.setMouseCursor(Cursor.DEFAULT)
        Init(project, editorContext)
      }
      def onMouseEntered(e: MouseEvent, imgSz: (Double, Double)): ModeEditor = this
      def onMouseExited(e: MouseEvent, imgSz: (Double, Double)): ModeEditor = this
      def onMouseMoved(e: MouseEvent, imgSz: (Double, Double)): ModeEditor = this
      def switchToAddMode(e: ActionEvent, imgSz: (Double, Double)): ModeEditor = AddMode.Init(project, editorContext)
      def switchToAddCropMode(e: ActionEvent, imgSz: (Double, Double)): ModeEditor = AddCropMode.Init(project, editorContext)
      def switchToSelectMode(e: ActionEvent, imgSz: (Double, Double)): ModeEditor = SelectMode.Init(project, editorContext)
    }

    case class SouthResize(
      project: Project,
      editorContext: EditorContext,
      p0: Point2D
    ) extends ModeEditor {
      def onMousePressed(e: MouseEvent, imgSz: (Double, Double)): ModeEditor = this
      def onMouseDragged(e: MouseEvent, imgSz: (Double, Double)): ModeEditor = {
        val p1 = new Point2D(e.getX, e.getY)
        project.southResizeSelectedFields(imgSz, p0, p1)
        SouthResize(project, editorContext, p1)
      }
      def onMouseReleased(e: MouseEvent, imgSz: (Double, Double)): ModeEditor = {
        editorContext.setMouseCursor(Cursor.DEFAULT)
        Init(project, editorContext)
      }
      def onMouseEntered(e: MouseEvent, imgSz: (Double, Double)): ModeEditor = this
      def onMouseExited(e: MouseEvent, imgSz: (Double, Double)): ModeEditor = this
      def onMouseMoved(e: MouseEvent, imgSz: (Double, Double)): ModeEditor = this
      def switchToAddMode(e: ActionEvent, imgSz: (Double, Double)): ModeEditor = AddMode.Init(project, editorContext)
      def switchToAddCropMode(e: ActionEvent, imgSz: (Double, Double)): ModeEditor = AddCropMode.Init(project, editorContext)
      def switchToSelectMode(e: ActionEvent, imgSz: (Double, Double)): ModeEditor = SelectMode.Init(project, editorContext)
    }

    case class NorthWestResize(
      project: Project,
      editorContext: EditorContext,
      p0: Point2D
    ) extends ModeEditor {
      def onMousePressed(e: MouseEvent, imgSz: (Double, Double)): ModeEditor = this
      def onMouseDragged(e: MouseEvent, imgSz: (Double, Double)): ModeEditor = {
        val p1 = new Point2D(e.getX, e.getY)
        project.northWestResizeSelectedFields(imgSz, p0, p1)
        NorthWestResize(project, editorContext, p1)
      }
      def onMouseReleased(e: MouseEvent, imgSz: (Double, Double)): ModeEditor = {
        editorContext.setMouseCursor(Cursor.DEFAULT)
        Init(project, editorContext)
      }
      def onMouseEntered(e: MouseEvent, imgSz: (Double, Double)): ModeEditor = this
      def onMouseExited(e: MouseEvent, imgSz: (Double, Double)): ModeEditor = this
      def onMouseMoved(e: MouseEvent, imgSz: (Double, Double)): ModeEditor = this
      def switchToAddMode(e: ActionEvent, imgSz: (Double, Double)): ModeEditor = AddMode.Init(project, editorContext)
      def switchToAddCropMode(e: ActionEvent, imgSz: (Double, Double)): ModeEditor = AddCropMode.Init(project, editorContext)
      def switchToSelectMode(e: ActionEvent, imgSz: (Double, Double)): ModeEditor = SelectMode.Init(project, editorContext)
    }

    case class NorthEastResize(
      project: Project,
      editorContext: EditorContext,
      p0: Point2D
    ) extends ModeEditor {
      def onMousePressed(e: MouseEvent, imgSz: (Double, Double)): ModeEditor = this
      def onMouseDragged(e: MouseEvent, imgSz: (Double, Double)): ModeEditor = {
        val p1 = new Point2D(e.getX, e.getY)
        project.northEastResizeSelectedFields(imgSz, p0, p1)
        NorthEastResize(project, editorContext, p1)
      }
      def onMouseReleased(e: MouseEvent, imgSz: (Double, Double)): ModeEditor = {
        editorContext.setMouseCursor(Cursor.DEFAULT)
        Init(project, editorContext)
      }
      def onMouseEntered(e: MouseEvent, imgSz: (Double, Double)): ModeEditor = this
      def onMouseExited(e: MouseEvent, imgSz: (Double, Double)): ModeEditor = this
      def onMouseMoved(e: MouseEvent, imgSz: (Double, Double)): ModeEditor = this
      def switchToAddMode(e: ActionEvent, imgSz: (Double, Double)): ModeEditor = AddMode.Init(project, editorContext)
      def switchToAddCropMode(e: ActionEvent, imgSz: (Double, Double)): ModeEditor = AddCropMode.Init(project, editorContext)
      def switchToSelectMode(e: ActionEvent, imgSz: (Double, Double)): ModeEditor = SelectMode.Init(project, editorContext)
    }

    case class SouthWestResize(
      project: Project,
      editorContext: EditorContext,
      p0: Point2D
    ) extends ModeEditor {
      def onMousePressed(e: MouseEvent, imgSz: (Double, Double)): ModeEditor = this
      def onMouseDragged(e: MouseEvent, imgSz: (Double, Double)): ModeEditor = {
        val p1 = new Point2D(e.getX, e.getY)
        project.southWestResizeSelectedFields(imgSz, p0, p1)
        SouthWestResize(project, editorContext, p1)
      }
      def onMouseReleased(e: MouseEvent, imgSz: (Double, Double)): ModeEditor = {
        editorContext.setMouseCursor(Cursor.DEFAULT)
        Init(project, editorContext)
      }
      def onMouseEntered(e: MouseEvent, imgSz: (Double, Double)): ModeEditor = this
      def onMouseExited(e: MouseEvent, imgSz: (Double, Double)): ModeEditor = this
      def onMouseMoved(e: MouseEvent, imgSz: (Double, Double)): ModeEditor = this
      def switchToAddMode(e: ActionEvent, imgSz: (Double, Double)): ModeEditor = AddMode.Init(project, editorContext)
      def switchToAddCropMode(e: ActionEvent, imgSz: (Double, Double)): ModeEditor = AddCropMode.Init(project, editorContext)
      def switchToSelectMode(e: ActionEvent, imgSz: (Double, Double)): ModeEditor = SelectMode.Init(project, editorContext)
    }

    case class SouthEastResize(
      project: Project,
      editorContext: EditorContext,
      p0: Point2D
    ) extends ModeEditor {
      def onMousePressed(e: MouseEvent, imgSz: (Double, Double)): ModeEditor = this
      def onMouseDragged(e: MouseEvent, imgSz: (Double, Double)): ModeEditor = {
        val p1 = new Point2D(e.getX, e.getY)
        project.southEastResizeSelectedFields(imgSz, p0, p1)
        SouthEastResize(project, editorContext, p1)
      }
      def onMouseReleased(e: MouseEvent, imgSz: (Double, Double)): ModeEditor = {
        editorContext.setMouseCursor(Cursor.DEFAULT)
        Init(project, editorContext)
      }
      def onMouseEntered(e: MouseEvent, imgSz: (Double, Double)): ModeEditor = this
      def onMouseExited(e: MouseEvent, imgSz: (Double, Double)): ModeEditor = this
      def onMouseMoved(e: MouseEvent, imgSz: (Double, Double)): ModeEditor = this
      def switchToAddMode(e: ActionEvent, imgSz: (Double, Double)): ModeEditor = AddMode.Init(project, editorContext)
      def switchToAddCropMode(e: ActionEvent, imgSz: (Double, Double)): ModeEditor = AddCropMode.Init(project, editorContext)
      def switchToSelectMode(e: ActionEvent, imgSz: (Double, Double)): ModeEditor = SelectMode.Init(project, editorContext)
    }

    case class Dragging(
      sw: SelectionWidget,
      project: Project,
      editorContext: EditorContext,
      p0: Point2D, p1: Point2D
    ) extends ModeEditor {
      def onMousePressed(e: MouseEvent, imgSz: (Double, Double)): ModeEditor = this
      def onMouseDragged(e: MouseEvent, imgSz: (Double, Double)): ModeEditor = {
        val newP1 = new Point2D(e.getX, e.getY)
        val newSw = SelectionWidget(toRect(p0, newP1))

        editorContext.redrawRect(sw.drawArea(imgSz))
        editorContext.drawWidget(newSw, false)
        Dragging(newSw, project, editorContext, p0, newP1)
      }
      def onMouseReleased(e: MouseEvent, imgSz: (Double, Double)): ModeEditor = {
        editorContext.setMouseCursor(Cursor.DEFAULT)
        val newP1 = new Point2D(e.getX, e.getY)
        editorContext.redrawRect(sw.drawArea(imgSz))
        editorContext.selectFields(toRect(p0, newP1), e)
        Init(project, editorContext)
      }
      def onMouseEntered(e: MouseEvent, imgSz: (Double, Double)): ModeEditor = this
      def onMouseExited(e: MouseEvent, imgSz: (Double, Double)): ModeEditor = this
      def onMouseMoved(e: MouseEvent, imgSz: (Double, Double)): ModeEditor = this
      def switchToAddMode(e: ActionEvent, imgSz: (Double, Double)): ModeEditor = AddMode.Init(project, editorContext)
      def switchToAddCropMode(e: ActionEvent, imgSz: (Double, Double)): ModeEditor = AddCropMode.Init(project, editorContext)
      def switchToSelectMode(e: ActionEvent, imgSz: (Double, Double)): ModeEditor = SelectMode.Init(project, editorContext)
    }
  }
}

class Editor(
  project: Project,
  editorContext: EditorContext
) {
  private var editor: ModeEditors.ModeEditor = ModeEditors.AddMode.Init(
    project, editorContext
  )

  def initialize() {
  }

  def onMousePressed(e: MouseEvent, imgSz: (Double, Double)) {
    editor = editor.onMousePressed(e, imgSz)
  }

  def onMouseDragged(e: MouseEvent, imgSz: (Double, Double)): Unit = {
    editor = editor.onMouseDragged(e, imgSz)
  }

  def onMouseReleased(e: MouseEvent, imgSz: (Double, Double)): Unit = {
    editor = editor.onMouseReleased(e, imgSz)
  }

  def onMouseExited(e: MouseEvent, imgSz: (Double, Double)): Unit = {
    editor = editor.onMouseExited(e, imgSz)
  }

  def onMouseEntered(e: MouseEvent, imgSz: (Double, Double)): Unit = {
    editor = editor.onMouseEntered(e, imgSz)
  }

  def onMouseMoved(e: MouseEvent, imgSz: (Double, Double)): Unit = {
    editor = editor.onMouseMoved(e, imgSz)
  }

  def switchToAddMode(e: ActionEvent, imgSz: (Double, Double)): Unit = {
    editor = editor.switchToAddMode(e, imgSz)
  }

  def switchToSelectMode(e: ActionEvent, imgSz: (Double, Double)): Unit = {
    editor = editor.switchToSelectMode(e, imgSz)
  }

  def switchToAddCropMode(e: ActionEvent, imgSz: (Double, Double)): Unit = {
    editor = editor.switchToAddCropMode(e, imgSz)
  }
}

case class SelectedImage(file: Path, image: Image) {
  val imageSize: (Double, Double) = (image.getWidth, image.getHeight)
}

class MainController extends Initializable with HandleBigJob {
  val ModuleServerUrl = "https://dev.functionalcapture.com"
  val pref: Preferences = Preferences.userNodeForPackage(getClass)
  val settingsLoader: SettingsLoader = Settings.Loader
  private var imageTable: ImageTable = new ImageTable
  private var stage: Stage = _
  private var project: Project = _
  private var selectedImage: Option[SelectedImage] = None
  private var editor: Editor = _
  @volatile private var projectConfigName: Option[String] = None

  def setStage(stage: Stage) {
    this.stage = stage
    stage.setWidth(1024)
    stage.setHeight(800)
    stage.scene().setOnKeyReleased(new EventHandler[KeyEvent] {
      override def handle(t: KeyEvent) {
        t.getCode() match {
          case KeyCode.DELETE =>
            project.deleteAllSelectedFields()
          case _ =>
        }
      }
    })

    hookWindowClose()
  }

  private[this] def hookWindowClose() {
    stage.onCloseRequest = new EventHandler[WindowEvent]() {
      def handle(e: WindowEvent) {
        if (project.isDirty) {
          val dlg = new SfxAlert(AlertType.Confirmation) {
            title = "保管確認"
            contentText = "帳票設定を保管しますか？"
            buttonTypes = Seq(
              SfxButtonType.Cancel, SfxButtonType.No, SfxButtonType.Yes
            )
          }
          dlg.showAndWait() match {
            case Some(SfxButtonType.Yes) =>
              saveProject(callbackOnCancel = () => e.consume())
              terminateApplication()
            case Some(SfxButtonType.No) =>
              terminateApplication()
            case _ =>
              e.consume()
          }
        }
        else {
          terminateApplication()
        }
      }
    }
  }

  private[this] def terminateApplication() {
    val dlg = new Alert(AlertType.None)
    dlg.setTitle("アプリケーション終了処理中")
    dlg.setContentText("少々お待ちください。終了処理中です。")
    dlg.show()

    Main.terminate()
  }

  private[this] def openProject() {
    val loader = new FXMLLoader(getClass().getResource("open.fxml"))
    val root: DialogPane = loader.load()
    val ctrl: OpenController = loader.getController().asInstanceOf[OpenController]
    val alert = new SfxAlert(AlertType.Confirmation) {
      title = "開く"
    }
    alert.dialogPane = new SfxDialogPane(root)
    ctrl.project = project
    ctrl.dialog = alert
    doBigJob {
      Right(project.listConfig())
    } {
      case ListConfigResultOk(resp) =>
        ctrl.formConfigs_=(resp.configTable)
        alert.showAndWait() match {
          case Some(button) =>
            if (button.buttonData.delegate == ButtonBar.ButtonData.APPLY) {
              ctrl.getSelectedConfig match {
                case None => {
                  val dlg = new SfxAlert(AlertType.Error) {
                    title = "選択エラー"
                    contentText = "開きたい構成を選択してください"
                  }
                  dlg.showAndWait()
                  openProject()
                }
                case Some(fc) =>
                  println("Open config: " + fc)
                  doBigJob {
                    Right(project.openConfig(fc.configName))
                  } {
                    case OpenConfigResultOk(resp) =>
                      redraw()
                    case authFail: RestAuthFailure =>
                      authError()
                    case serverFail: RestUnknownFailure =>
                      showGeneralError()
                  }
              }
            }
          case _ =>
        }
      case authFail: RestAuthFailure =>
        authError()
      case serverFail: RestUnknownFailure =>
        showGeneralError()
    }
  }

  private[this] def saveProject(callbackOnCancel: () => Unit = () => {}) {
    val loader = new FXMLLoader(getClass().getResource("save.fxml"))
    val root: DialogPane = loader.load()
    val ctrl: SaveController = loader.getController().asInstanceOf[SaveController]
    val alert = new SfxAlert(AlertType.Confirmation) {
      title = "保管"
    }
    alert.dialogPane = new SfxDialogPane(root)
    projectConfigName.foreach { cname =>
      ctrl.configName = cname
    }
    ctrl.project = project

    doBigJob {
      Right(project.listConfig())
    } {
      case ListConfigResultOk(resp) =>
        ctrl.formConfigs_=(resp.configTable)
        alert.showAndWait() match {
          case Some(button) =>
            if (button.buttonData.delegate == ButtonBar.ButtonData.APPLY) {
              projectConfigName = Some(ctrl.configName)
              doBigJob {
                Right(project.saveConfig(ctrl.configName))
              } {
                case SaveConfigResultOk(resp) =>
                  val dlg = new SfxAlert(AlertType.Information) {
                    title = "保管成功"
                    contentText = "保管しました。リビジョン: " + resp.revision.value
                  }
                  dlg.showAndWait()
                case authFail: RestAuthFailure =>
                  authError()
                  callbackOnCancel()
                case serverFail: RestUnknownFailure =>
                  showGeneralError()
                  callbackOnCancel()
              }
            }
          case _ =>
        }
      case authFail: RestAuthFailure =>
        authError()
        callbackOnCancel()
      case serverFail: RestUnknownFailure =>
        showGeneralError()
        callbackOnCancel()
    }
  }

  @FXML
  private[this] var imageListView: ListView[File] = _

  @FXML
  private[this] var imageCanvas: Canvas = _

  @FXML
  private[this] var cropCheck: CheckBox = _

  lazy val sfxCropCheck = new SfxCheckBox(cropCheck)

  @FXML
  private[this] var dotRemovalCheck: CheckBox = _

  lazy val sfxDotRemovalCheck = new SfxCheckBox(dotRemovalCheck)

  @FXML
  private[this] var cropRectangleCheck: CheckBox = _

  lazy val sfxCropRectangleCheck = new SfxCheckBox(cropRectangleCheck)

  @FXML
  private[this] var removeRuledLineCheck: CheckBox = _

  lazy val sfxRemoveRuledLineCheck = new SfxCheckBox(removeRuledLineCheck)

  @FXML
  private[this] var skewCorrectionCheck: CheckBox = _

  lazy val sfxSkewCorrectionCheck = new SfxCheckBox(skewCorrectionCheck)

  @FXML
  private[this] var addModeButton: ToggleButton = _

  @FXML
  private[this] var selectModeButton: ToggleButton = _

  lazy val sfxImageListView = new SfxListView(imageListView.asInstanceOf[ListView[File]])
  lazy val sfxImageCanvas = new SfxCanvas(imageCanvas)

  def redrawRect(rect: Rectangle2D): Unit = {
    println("redrawRect(" + rect + ")")
    val gc = sfxImageCanvas.graphicsContext2D
    selectedImage.foreach { si =>
      doBigJob(project.cachedImage(si)) {
        case ok: RetrievePreparedImageResultOk =>
          ok.serverResp.foreach { prepareResult =>
            prepareResult.cropResult.foreach { cr =>
              cr match {
                case s: CropResultSuccess =>
                case CropResultCannotFindEdge =>
                  project.cropEnabled = false
                  showCropError()
              }
            }
          }

          val x = math.floor(rect.minX)
          val w = math.ceil(rect.width + 1)
          val y = math.floor(rect.minY)
          val h = math.ceil(rect.height + 1)
          gc.drawImage(
            ok.image,
            x, y, w, h,
            x, y, w, h
          )

          val imgSz = (ok.image.width.get, ok.image.height.get)
          if (! project.cropEnabled) {
            project.leftCropField.foreach { cf =>
              if (cf.intersects(imgSz, rect)) {
                cf.draw(imgSz, gc, project.isLeftCropFieldSelected)
              }
            }
            project.topCropField.foreach { cf =>
              if (cf.intersects(imgSz, rect)) {
                cf.draw(imgSz, gc, project.isTopCropFieldSelected)
              }
            }
            project.rightCropField.foreach { cf =>
              if (cf.intersects(imgSz, rect)) {
                cf.draw(imgSz, gc, project.isRightCropFieldSelected)
              }
            }
            project.bottomCropField.foreach { cf =>
              if (cf.intersects(imgSz, rect)) {
                cf.draw(imgSz, gc, project.isBottomCropFieldSelected)
              }
            }
          }
          project.absoluteFields.normalFields.foreach { af =>
            if (af.intersects(imgSz, rect)) {
              af.draw(imgSz, gc, false)
            }
          }
          project.absoluteFields.selectedFields.foreach { af =>
            if (af.intersects(imgSz, rect)) {
              af.draw(imgSz, gc, true)
            }
          }
        case authFail: RestAuthFailure =>
          sfxSkewCorrectionCheck.selected = false
          project.skewCorrection = project.skewCorrection.copy(enabled = false)
          sfxCropCheck.selected = false
          project.cropEnabled = false
          authError()
        case serverFail: RestUnknownFailure =>
          sfxSkewCorrectionCheck.selected = false
          project.skewCorrection = project.skewCorrection.copy(enabled = false)
          sfxCropCheck.selected = false
          project.cropEnabled = false
          showGeneralError()
      }
    }
  }

  def doWithImageSize(f: ((Double, Double)) => Unit) {
    selectedImage.foreach { si =>
      doBigJob(project.cachedImage(si)) {
        case ok: RetrievePreparedImageResultOk =>
          ok.serverResp.foreach { prepareResult =>
            prepareResult.cropResult.foreach { cr =>
              cr match {
                case s: CropResultSuccess =>
                case CropResultCannotFindEdge =>
                  project.cropEnabled = false
                  showCropError()
              }
            }
          }
          val imgSz = (ok.image.width.get, ok.image.height.get)
          f(imgSz)
        case authFail: RestAuthFailure =>
          sfxSkewCorrectionCheck.selected = false
          project.skewCorrection = project.skewCorrection.copy(enabled = false)
          sfxCropCheck.selected = false
          project.cropEnabled = false
          authError()
        case serverFail: RestUnknownFailure =>
          sfxSkewCorrectionCheck.selected = false
          project.skewCorrection = project.skewCorrection.copy(enabled = false)
          sfxCropCheck.selected = false
          project.cropEnabled = false
          showGeneralError()
      }
    }
  }

  def drawWidget(widget: Widget[_], isSelected: Boolean): Unit = {
    doWithImageSize { sz =>
      widget.draw(sz, sfxImageCanvas.graphicsContext2D, isSelected)
    }
  }

  def orgImageSize: (Double, Double) = selectedImage.get.imageSize

  def drawSelectionRect(rect: Rectangle2D): Unit = {
    println("drawSelectionRect(" + rect + ")")
    val gc = sfxImageCanvas.graphicsContext2D
    gc.setLineWidth(2.0)
    gc.setLineDashes()
    gc.setStroke(Color.BLACK)
    gc.setLineDashes(5.0, 5.0)
    gc.strokeRect(rect.minX, rect.minY, rect.width, rect.height)
  }

  def setMouseCursor(cursor: Cursor) {
    stage.scene.get().setCursor(cursor)
  }

  def rectangle2D(x0: Double, y0: Double, x1: Double, y1: Double): Rectangle2D = {
    val (minX, maxX) = if (x0 < x1) (x0, x1) else (x1, x0)
    val (minY, maxY) = if (y0 < y1) (y0, y1) else (y1, y0)
    new Rectangle2D(minX, minY, maxX - minX, maxY - minY)
  }

  def fieldCreated(field: Field): Unit = {
    println("fieldCreated(" + field + ")")

    doWithImageSize { imgSz =>
      field match {
        case af: AbsoluteField => {
          val (dlg, ctrl) = absoluteFieldDialog
          dlg.showAndWait().map(_.delegate) match {
            case Some(ButtonType.APPLY) =>
              project.addAbsoluteField(af.withName(ctrl.fieldName), true)
              println("APPLY")
            case Some(_) =>
              println("canceled")
              redrawRect(field.drawArea(imgSz))
            case None => println("bt = none")
          }
        }
          // val dlg = new SfxTextInputDialog("")
          // dlg.title = "フィールドの名前入力"
          // dlg.headerText = "フィールドの名前を入力してください。"
          // dlg.showAndWait() match {
          //   case None =>
          //     redrawRect(field.drawArea(imgSz))
          //   case Some(name) =>
          //     project.addAbsoluteField(af.withName(name), true)
          // }
        case cf: CropField =>
          val Left = "左余白検出"
          val Right = "右余白検出"
          val Top = "上余白検出"
          val Bottom = "下余白検出"
          val dlg = new SfxChoiceDialog(Top, Seq(Top, Left, Right, Bottom))
          dlg.title = "余白位置の選択"
          dlg.headerText = "余白位置を選択してください"
          dlg.showAndWait() match {
            case None =>
              redrawRect(field.drawArea(imgSz))
            case Some(loc) => (loc match {
              case Left => project.addLeftCropField(cf.toLeft, true)
              case Right => project.addRightCropField(cf.toRight, true)
              case Top => project.addTopCropField(cf.toTop, true)
              case Bottom => project.addBottomCropField(cf.toBottom, true)
            }).foreach { oldCropField =>
              redrawRect(oldCropField.drawArea(imgSz))
            }
          }
      }
    }
  }

  def selectFields(rect: Rectangle2D, e: MouseEvent): Unit = {
    println("selectFields(" + rect + ", " + e + ")")
    doWithImageSize { imgSz =>
      project.selectAbsoluteFields(imgSz, rect, e)
      project.selectCropFields(imgSz, rect, e)
    }
  }

  @FXML
  def exitMenuClicked(event: ActionEvent) {
    println("exitMenuClicked()")
    stage.getOnCloseRequest().handle(
      new WindowEvent(
        stage,
        WindowEvent.WINDOW_CLOSE_REQUEST
      )
    )
  }

  @FXML
  def saveMenuClicked(event: ActionEvent) {
    println("saveMenuClicked()")
    saveProject()
  }

  @FXML
  def openMenuClicked(event: ActionEvent) {
    println("openMenuClicked()")
    openProject()
  }

  @FXML
  def imageOpenMenuClicked(event: ActionEvent) {
    val fc = new FileChooser {
      title = "Select image file"
      extensionFilters.add(new FileChooser.ExtensionFilter("Image Files", Seq("*.png", "*.tif", "*.pdf")))
      initialDirectory = new File(
        pref.get("imageDirectory", System.getProperty("user.home"))
      )
    }

    Option(fc.showOpenMultipleDialog(stage)).foreach { ftbl =>
      @tailrec def grouped(
        files: imm.Seq[File], pngFiles: Vector[File], pdfFiles: Vector[File], tifFiles: Vector[File]
      ): (Vector[File], Vector[File], Vector[File]) = {
        if (files.isEmpty) (pngFiles, pdfFiles, tifFiles)
        else {
          val head = files.head
          val fileName = head.getName.toLowerCase
          if (fileName.endsWith(".pdf")) grouped(files.tail, pngFiles, pdfFiles :+ head, tifFiles)
          else if (fileName.endsWith(".tif")) grouped(files.tail, pngFiles, pdfFiles, tifFiles :+ head)
          else grouped(files.tail, pngFiles :+ head, pdfFiles, tifFiles)
        }
      }

      val (pngFiles: Vector[File], pdfFiles: Vector[File], tifFiles: Vector[File]) = grouped(
        ftbl.toList, Vector(), Vector(), Vector()
      )

      imageTable.addFiles(pngFiles)
      (pngFiles ++ pdfFiles ++ tifFiles).headOption.foreach { f =>
        pref.put("imageDirectory", f.getParentFile.getAbsolutePath)
      }

      if (! pdfFiles.isEmpty || ! tifFiles.isEmpty) {
        val alert = new SfxAlert(AlertType.Confirmation)
        alert.title = "ファイルの変換"
        alert.contentText = "pdf/tiffファイルは、pngファイルに変換します。"
        alert.showAndWait().map(_.delegate) match {
          case Some(ButtonType.OK) =>
            println("Convert confirmed")
            doBigJob {
              convertFiles(pdfFiles ++ tifFiles)
            } { (results: imm.Seq[ConvertFilesResult]) =>
              imageTable.addFiles(
                results.foldLeft(Vector[File]()) { (sum, r) =>
                  r match {
                    case ok: ConvertFilesResultOk =>
                      sum ++ ok.files
                    case authFail: ConvertFilesAuthError =>
                      authError()
                      sum
                    case serverFail: ConvertFilesUnknownError =>
                      showGeneralError()
                      sum
                  }
                }
              )
            }
          case Some(btn) =>
            println("Button: " + btn)
          case None =>
            println("Canceled")
        }
      }
    }
  }

  def convertFiles(files: imm.Seq[File]): Either[imm.Seq[ConvertFilesResult], Future[imm.Seq[ConvertFilesResult]]] = {
    val auth = Settings.Loader.settings.auth

    Right(
      Future {
        val urlPath = Settings.Loader.settings.auth.url.resolve("convert")

        files.map { (f: File) =>
          PathUtil.withTempFile(None, None) { (zipFileToSubmit: Path) =>
            val json = Json.obj(
              "inputFile" -> f.getName
            )
            PathUtil.withTempFile(None, None) { (configFile: Path) =>
              Files.write(configFile, json.toString.getBytes("utf-8"))
              Zip.deflate(
                zipFileToSubmit,
                Seq(
                  "config.json" -> configFile,
                  f.getName -> f.toPath
                )
              )

              Await.result(
                Ws().url(urlPath).withRequestTimeout(5.minutes).addHttpHeaders(
                  "Content-Type" -> "application/zip",
                  "Authorization" -> (auth.contractedUserId.value + "_" + auth.applicationToken.value)
                ).post(
                  zipFileToSubmit.toFile
                ).map { resp =>
                  println("status = " + resp.status)
                  println("statusText = " + resp.statusText)
                  resp.status match {
                    case 200 =>
                      PathUtil.withTempFile(None, None) { zipFile =>
                        using(FileChannel.open(zipFile, StandardOpenOption.CREATE, StandardOpenOption.WRITE)) { ch =>
                          ch.write(resp.bodyAsBytes.toByteBuffer)
                        }.get
                        val out: Path = Files.createTempDirectory(null)
                        OnShutdown.addDeleteDirectory(out)
                        println("Convert result: " + out)
                        Zip.explode(zipFile, out)
                        new ConvertFilesResultOk(out.toFile.listFiles().toList)
                      }.get
                    case 403 =>
                      ConvertFilesAuthError(resp.status, resp.statusText, resp.body)
                    case _ =>
                      ConvertFilesUnknownError(resp.status, resp.statusText, resp.body, f.toPath)
                  }
                },
                Duration.apply(60, TimeUnit.SECONDS)
              )
            }.get
          }.get
        }
      }
    )
  }

  @FXML
  def imageSelected(e: MouseEvent) {
    val imageFile = e.getSource().asInstanceOf[ListView[File]].getSelectionModel().getSelectedItem()
    println("Image selected " + imageFile)
    if (imageFile != null) {
      val img = new Image(imageFile.toURI().toString())
      selectedImage = Some(SelectedImage(imageFile.toPath, img))

      redraw()
    }
  }

  def showCropError() {
    val err = new Alert(AlertType.Error)
    err.setTitle("余白切り取りにエラー")
    err.setContentText("余白の切り取りに失敗しました。切り取り範囲を確認してください。")
    err.show()
  }

  def redraw(onError: Option[Throwable => Unit] = None) {
    println("redraw()")
    selectedImage.foreach { selectedImage =>
      doBigJob(
        project.cachedImage(selectedImage),
        { t =>
          onError match {
            case Some(h) =>
              h(t)
              showGeneralError()
            case None => showGeneralError()
          }
        }
      ) {
        case ok: RetrievePreparedImageResultOk =>
          val img = ok.image

          sfxImageCanvas.width = img.width.toDouble
          sfxImageCanvas.height = img.height.toDouble
          val ctx = sfxImageCanvas.graphicsContext2D
          ctx.clearRect(0, 0, sfxImageCanvas.width.toDouble, sfxImageCanvas.height.toDouble)
          ctx.drawImage(img, 0, 0)
          ok.serverResp.foreach { prepareResult =>
            prepareResult.cropResult.foreach { cr =>
              cr match {
                case s: CropResultSuccess =>
                case CropResultCannotFindEdge =>
                  project.cropEnabled = false
                  showCropError()
              }
            }
          }
          project.redraw()
        case authFail: RestAuthFailure =>
          sfxSkewCorrectionCheck.selected = false
          project.skewCorrection = project.skewCorrection.copy(enabled = false)
          sfxCropCheck.selected = false
          project.cropEnabled = false
          authError()
        case serverFail: RestUnknownFailure =>
          sfxSkewCorrectionCheck.selected = false
          project.skewCorrection = project.skewCorrection.copy(enabled = false)
          sfxCropCheck.selected = false
          project.cropEnabled = false
          showGeneralError()
      }
    }
  }

  @FXML
  def dotRemovalDetailClicked(e: ActionEvent) {
    println("dotRemovalDetailClicked")
    val loader = new FXMLLoader(getClass().getResource("dotRemoval.fxml"))
    val root: DialogPane = loader.load()
    val ctrl = loader.getController().asInstanceOf[DotRemovalDetailController]
    ctrl.model = project.dotRemoval.condition
    val alert = new SfxAlert(AlertType.Confirmation)
    alert.dialogPane = new SfxDialogPane(root)
    alert.title = "黒点削除"
    alert.onCloseRequest = new EventHandler[DialogEvent] {
      override def handle(t: DialogEvent) {
        if (t.getTarget.asInstanceOf[Alert].getResult.getButtonData == ButtonBar.ButtonData.CANCEL_CLOSE) return

        ctrl.validate match {
          case None =>
          case Some(MaxDotSizeInvalid) =>
            val dlg = new SfxAlert(AlertType.Information) {
              title = "黒点最大サイズ エラー"
              contentText = "黒点最大サイズは、1以上の整数で指定してください"
            }
            dlg.showAndWait()
            t.consume()
          case Some(BlackLevelInvalid) =>
            val dlg = new SfxAlert(AlertType.Information) {
              title = "黒レベル エラー"
              contentText = "黒レベルは、0-254で指定してください"
            }
            dlg.showAndWait()
            t.consume()
        }
      }
    }
    alert.showAndWait().map(_.delegate) match {
      case Some(ButtonType.APPLY) =>
        val model = ctrl.model
        println("apply dot removal detail " + model)
        project.dotRemoval = project.dotRemoval.copy(condition = model)
        project.invalidateCachedImage(
          CacheConditionGlob(
            isDotRemovalEnabled = Some(true)
          )
        )
        if (sfxDotRemovalCheck.selected()) {
          sfxDotRemovalCheck.selected = false
          dotRemovalEnabledClicked(null)
          sfxDotRemovalCheck.selected = true
          dotRemovalEnabledClicked(null)
        }

      case Some(_) => println("canceled")
      case None => println("bt = none")
    }
  }

  @FXML
  def cropRectangleDetailClicked(e: ActionEvent) {
    println("cropRectangleDetailClicked")
    val loader = new FXMLLoader(getClass().getResource("cropRectangle.fxml"))
    val root: DialogPane = loader.load()
    val ctrl = loader.getController().asInstanceOf[CropRectangleDetailController]
    ctrl.model = project.cropRectangle.condition
    val alert = new SfxAlert(AlertType.Confirmation)
    alert.dialogPane = new SfxDialogPane(root)
    alert.title = "長方形クロップ"
    alert.onCloseRequest = new EventHandler[DialogEvent] {
      override def handle(t: DialogEvent) {
        if (t.getTarget.asInstanceOf[Alert].getResult.getButtonData == ButtonBar.ButtonData.CANCEL_CLOSE) return

        ctrl.validate match {
          case Some(InvalidErrorAllowance) =>
            val dlg = new SfxAlert(AlertType.Error) {
              title = "線途切れ許容量 エラー"
              contentText = "線途切れ許容量は、0以上の整数で指定してください"
            }
            dlg.showAndWait()
            t.consume()
          case Some(InvalidTopMargin) =>
            val dlg = new SfxAlert(AlertType.Error) {
              title = "上方向マージン割合 エラー"
              contentText = "0以上の数値で指定してください"
            }
            dlg.showAndWait()
            t.consume()
          case Some(InvalidLeftMargin) =>
            val dlg = new SfxAlert(AlertType.Error) {
              title = "左方向マージン割合 エラー"
              contentText = "0以上の数値で指定してください"
            }
            dlg.showAndWait()
            t.consume()
          case Some(InvalidRightMargin) =>
            val dlg = new SfxAlert(AlertType.Error) {
              title = "右方向マージン割合 エラー"
              contentText = "0以上の数値で指定してください"
            }
            dlg.showAndWait()
            t.consume()
          case Some(InvalidBottomMargin) =>
            val dlg = new SfxAlert(AlertType.Error) {
              title = "下方向マージン割合 エラー"
              contentText = "0以上の数値で指定してください"
            }
            dlg.showAndWait()
            t.consume()
          case Some(InvalidSlantAllowance) =>
            val dlg = new SfxAlert(AlertType.Error) {
              title = "傾斜許容ドット数 エラー"
              contentText = "0以上、" + CropRectangleDetailController.SlantAllowanceMax + "までの数値で指定してください"
            }
            dlg.showAndWait()
            t.consume()
          case None =>
        }
      }
    }
    alert.showAndWait().map(_.delegate) match {
      case Some(ButtonType.APPLY) =>
        val model = ctrl.model
        println("apply crop rectangle detail " + model)
        project.cropRectangle = project.cropRectangle.copy(condition = model)
        project.invalidateCachedImage(
          CacheConditionGlob(
            isCropRectangleEnabled = Some(true)
          )
        )
        if (sfxCropRectangleCheck.selected()) {
          sfxCropRectangleCheck.selected = false
          cropRectangleEnabledClicked(null)
          sfxCropRectangleCheck.selected = true
          cropRectangleEnabledClicked(null)
        }

      case Some(_) => println("canceled")
      case None => println("bt = none")
    }
  }

  @FXML
  def removeRuledLineDetailClicked(e: ActionEvent) {
    println("removeRuledLineDetail")
    val loader = new FXMLLoader(getClass().getResource("removeRuledLine.fxml"))
    val root: DialogPane = loader.load()
    val ctrl = loader.getController().asInstanceOf[RemoveRuledLineDetailController]
    ctrl.model = project.removeRuledLine.condition
    val alert = new SfxAlert(AlertType.Confirmation)
    alert.dialogPane = new SfxDialogPane(root)
    alert.title = "罫線削除"
    alert.onCloseRequest = new EventHandler[DialogEvent] {
      override def handle(t: DialogEvent) {
        if (t.getTarget.asInstanceOf[Alert].getResult.getButtonData == ButtonBar.ButtonData.CANCEL_CLOSE) return

        ctrl.validate match {
          case Some(InvalidLineDeltaX) =>
            val dlg = new SfxAlert(AlertType.Error) {
              title = "垂直線の太さ エラー"
              contentText = "垂直線の太さは、1以上の整数で指定してください"
            }
            dlg.showAndWait()
            t.consume()
          case Some(InvalidLineDeltaY) =>
            val dlg = new SfxAlert(AlertType.Error) {
              title = "水平線の太さ エラー"
              contentText = "水平線の太さは、1以上の整数で指定してください"
            }
            dlg.showAndWait()
            t.consume()
          case Some(InvalidLineDotRatio) =>
            val dlg = new SfxAlert(AlertType.Error) {
              title = "線とみなす黒点割合(%) エラー"
              contentText = "線とみなす黒点割合(%)は、1以上100以下の整数で指定してください"
            }
            dlg.showAndWait()
            t.consume()
          case Some(InvalidCorrectOverlappingDelta) =>
            val dlg = new SfxAlert(AlertType.Error) {
              title = "重複判断領域の幅 エラー"
              contentText = "重複判断領域の幅は、1以上の整数で指定してください"
            }
            dlg.showAndWait()
            t.consume()
          case Some(InvalidCorrectOverlappingDotRatio) =>
            val dlg = new SfxAlert(AlertType.Error) {
              title = "重複とみなす黒点割合(%) エラー"
              contentText = "重複とみなす黒点割合(%)は、1以上100以下の整数で指定してください"
            }
            dlg.showAndWait()
            t.consume()
          case None =>
        }
      }
    }
    alert.showAndWait().map(_.delegate) match {
      case Some(ButtonType.APPLY) =>
        val model = ctrl.model
        println("apply remove ruled line detail " + model)
        project.removeRuledLine = project.removeRuledLine.copy(condition = model)
        project.invalidateCachedImage(
          CacheConditionGlob(
            isRemoveRuledLineEnabled = Some(true)
          )
        )
        if (sfxRemoveRuledLineCheck.selected()) {
          sfxRemoveRuledLineCheck.selected = false
          removeRuledLineEnabledClicked(null)
          sfxRemoveRuledLineCheck.selected = true
          removeRuledLineEnabledClicked(null)
        }

      case Some(_) => println("canceled")
      case None => println("bt = none")
    }
  }

  @FXML
  def skewCorrectionDetailClicked(e: ActionEvent) {
    println("skewCorrectionDetail")
    val loader = new FXMLLoader(getClass().getResource("skewCorrectionDialog.fxml"))
    val root: DialogPane = loader.load()
    val ctrl = loader.getController().asInstanceOf[SkewCorrectionDetailController]
    ctrl.model = project.skewCorrection.condition
    val alert = new SfxAlert(AlertType.Confirmation)
    alert.dialogPane = new SfxDialogPane(root)
    alert.title = "傾き補正"
    alert.onCloseRequest = new EventHandler[DialogEvent] {
      override def handle(t: DialogEvent) {
        println("event type = " + t.getEventType)
        println("target = " + t.getTarget)
        println("result = " + t.getTarget.asInstanceOf[Alert].getResult)
//        t.consume()
      }
    }
    alert.showAndWait().map(_.delegate) match {
      case Some(ButtonType.APPLY) =>
        val model = ctrl.model
        println("apply skew correction detail " + model)
        project.skewCorrection = SkewCorrection(project.skewCorrection.enabled, model)
        project.invalidateCachedImage(
          CacheConditionGlob(
            isSkewCorrectionEnabled = Some(true)
          )
        )
        if (sfxSkewCorrectionCheck.selected()) {
          sfxSkewCorrectionCheck.selected = false
          skewCorrectionEnabledClicked(null)
          sfxSkewCorrectionCheck.selected = true
          skewCorrectionEnabledClicked(null)
        }

      case Some(_) => println("canceled")
      case None => println("bt = none")
    }
  }

  @FXML
  def cropDetailClicked(e: ActionEvent) {
    println("cropDetailClicked")
    val loader = new FXMLLoader(getClass().getResource("cropDetailDialog.fxml"))
    val root: DialogPane = loader.load()
    val ctrl = loader.getController().asInstanceOf[CropDetailController]
    ctrl.topSensivity = project.topEdgeCropSensivity
    ctrl.bottomSensivity = project.bottomEdgeCropSensivity
    ctrl.leftSensivity = project.leftEdgeCropSensivity
    ctrl.rightSensivity = project.rightEdgeCropSensivity
    val alert = new SfxAlert(AlertType.Confirmation)
    alert.dialogPane = new SfxDialogPane(root)
    alert.title = "傾き補正"
    alert.onCloseRequest = new EventHandler[DialogEvent] {
      override def handle(t: DialogEvent) {
        var errors: imm.Seq[String] = List()
        def parse(fieldName: String, f: => EdgeCropSensivity): Option[EdgeCropSensivity] = {
          try {
            Some(f)
          }
          catch {
            case t: Throwable =>
              t.printStackTrace()
              errors = errors :+ fieldName
              None
          }
        }

        parse("感度(上)", ctrl.topSensivity).foreach { project.topEdgeCropSensivity = _ }
        parse("感度(下)", ctrl.bottomSensivity).foreach { project.bottomEdgeCropSensivity = _ }
        parse("感度(左)", ctrl.leftSensivity).foreach { project.leftEdgeCropSensivity = _ }
        parse("感度(右)", ctrl.rightSensivity).foreach { project.rightEdgeCropSensivity = _ }

        if (! errors.isEmpty && t.getTarget.asInstanceOf[Alert].getResult.getButtonData != ButtonBar.ButtonData.CANCEL_CLOSE) {
          t.consume()
          val err = new Alert(AlertType.Error)
          err.setTitle("感度の指定エラー")
          err.setContentText("感度は、半角数字で、0-255の範囲で指定しください。" + errors.mkString(", "))
          err.show()
        }

        println("event type = " + t.getEventType)
        println("target = " + t.getTarget)
        println("result = " + t.getTarget.asInstanceOf[Alert].getResult)
      }
    }
    alert.showAndWait().map(_.delegate) match {
      case Some(ButtonType.APPLY) =>
        project.invalidateCachedImage(CacheConditionGlob(isCropEnabled = Some(true)))
        if (sfxCropCheck.selected()) {
          sfxCropCheck.selected = false
          cropEnabledCheckClicked(null)
          sfxCropCheck.selected = true
          cropEnabledCheckClicked(null)
        }

      case Some(_) => println("canceled")
      case None => println("bt = none")
    }
  }

  private def showSkewAnimation(skewResult: SkewCorrectionResult, image: Image) {
    val orgCropEnabled = project.cropEnabled
    project.skewCorrection = project.skewCorrection.copy(enabled = false)
    project.cropEnabled = false
    redraw()

    class ShowResultAnimation(
      val lines: Seq[(Double, Double, Double, Double)]
    ) extends AnimationTimer {
      val LineWidth = 2.0
      private var startNano: Long = 0L
      private var state = -1L
      val union: Rectangle2D = {
        var minX = Double.MaxValue
        var maxX = Double.MinValue
        var minY = Double.MaxValue
        var maxY = Double.MinValue

        lines.foreach { l =>
          if (l._1 < minX) minX = l._1
          if (l._3 < minX) minX = l._3
          if (l._2 < minY) minY = l._2
          if (l._4 < minY) minY = l._4

          if (maxX < l._1) maxX = l._1
          if (maxX < l._3) maxX = l._3
          if (maxY < l._2) maxY = l._2
          if (maxY < l._4) maxY = l._4
        }

        val w = maxX - minX + LineWidth + 2
        val h = maxY - minY + LineWidth + 2
        new Rectangle2D(minX - 1, minY - 1, if (w < 0) 0 else w, if (h < 0) 0 else h)
      }

      override def handle(nano: Long) {
        if (startNano == 0) startNano = nano

        val currentState = (nano - startNano) / 1000 / 1000 / 500
        if (currentState != state) {
          state = currentState
          val gc = sfxImageCanvas.graphicsContext2D
          gc.setLineWidth(LineWidth)
          gc.setLineDashes()
          gc.setStroke(Color.RED)
          gc.setLineDashes(5.0, 5.0)

          if ((state % 2) == 0) {
            lines.foreach { l =>
              println("draw line " + l)
              gc.strokeLine(l._1, l._2, l._3, l._4)
            }
          }
          else {
            println("clear line " + union)
            redrawRect(union)
          }

          if (3 <= currentState) {
            stop()
            project.skewCorrection = project.skewCorrection.copy(enabled = true)
            project.cropEnabled = orgCropEnabled
            redraw()
          }
        }
      }
    }

    val showLines: Seq[(Double, Double, Double, Double)] = skewResult.foundLines.map { l =>
      val ro = l.ro
      val th = l.theta + Pi / 2
      println("ro = " + ro + ", th = " + th)

      if (Pi / 2 - 0.1 < th && th < Pi / 2 + 0.1) { // horizontal
        val line = NearlyHorizontalLine(ro, th)
        val w = image.width.get()

        (0d, line.y(0), w, line.y(w))
      }
      else {
        val line = NearlyVerticalLine(ro, th)
        val h = image.height.get()

        (line.x(0), 0d, line.x(h), h)
      }
    }

    new ShowResultAnimation(showLines).start()
  }

  @FXML
  def dotRemovalEnabledClicked(e: ActionEvent) {
    println("dotRemovalEnabledClicked")
    project.dotRemoval = project.dotRemoval.copy(enabled = sfxDotRemovalCheck.selected())
    redraw(
      Some((t: Throwable) => project.dotRemoval.copy(enabled = false))
    )
  }

  @FXML
  def cropRectangleEnabledClicked(e: ActionEvent) {
    println("cropRectangleEnabledClicked")
    project.cropRectangle = project.cropRectangle.copy(enabled = sfxCropRectangleCheck.selected())
    redraw(
      Some((t: Throwable) => project.cropRectangle.copy(enabled = false))
    )
  }

  @FXML
  def removeRuledLineEnabledClicked(e: ActionEvent) {
    println("removeRuledLineEnabledClicked")
    project.removeRuledLine = project.removeRuledLine.copy(enabled = sfxRemoveRuledLineCheck.selected())
    redraw(
      Some((t: Throwable) => project.removeRuledLine.copy(enabled = false))
    )
  }

  @FXML
  def skewCorrectionEnabledClicked(e: ActionEvent) {
    println(
      "skewCorrectionEnabledClicked sfxSkewCorrectionCheck.selected() = " + sfxSkewCorrectionCheck.selected() +
        ", project.skewCorrection.enabled = " + project.skewCorrection.enabled
    )

    if (sfxSkewCorrectionCheck.selected() == project.skewCorrection.enabled) return

    selectedImage.foreach { si =>
      try {
        if (sfxSkewCorrectionCheck.selected()) {
          doBigJob(
            project.cachedImage(
              si,
              CacheCondition(
                isSkewCorrectionEnabled = true,
                isCropEnabled = project.cropEnabled,
                isDotRemovalEnabled = project.dotRemoval.enabled,
                isCropRectangleEnabled = project.cropRectangle.enabled,
                isRemoveRuledLineEnabled = project.removeRuledLine.enabled
              )
            ),
            { t =>
              sfxSkewCorrectionCheck.selected = false
              showGeneralError()
            }
          ) {
            case result: RetrievePreparedImageResultOk =>
              result.serverResp.foreach { pr =>
                pr.skewCorrectionResult.foreach { sr =>
                  showSkewAnimation(sr, si.image)
                }
              }
            case authFail: RestAuthFailure =>
              sfxSkewCorrectionCheck.selected = false
              authError()
            case unknownError: RestUnknownFailure =>
              sfxSkewCorrectionCheck.selected = false
              showGeneralError()
          }
        }
        else {
          project.skewCorrection = project.skewCorrection.copy(enabled = false)
          redraw()
        }
      }
      catch {
        case t: Throwable =>
          t.printStackTrace()
          sfxSkewCorrectionCheck.selected = false
          sfxCropCheck.selected = false
      }
    }
  }

  @FXML
  def cropEnabledCheckClicked(e: ActionEvent) {
    println("sfxCropCheck.selected() = " + sfxCropCheck.selected() + ", project.cropEnabled = " + project.cropEnabled)
    if (sfxCropCheck.selected() && ! project.cropFieldsAreReady) {
      sfxCropCheck.selected = false
      val err = new Alert(AlertType.Error)
      err.setTitle("操作エラー")
      err.setContentText("余白検出領域を4箇所全て設定してください。")
      err.show()
    }
    else {
      project.cropEnabled = sfxCropCheck.selected()
      redraw(
        Some((t: Throwable) => project.cropEnabled = false)
      )
    }
  }

  @FXML
  def addModeClicked(e: ActionEvent) {
    println("addModeClicked")
    doWithImageSize { imgSz =>
      editor.switchToAddMode(e, imgSz)
    }
  }

  @FXML
  def selectModeClicked(e: ActionEvent) {
    println("selectModeClicked")
    doWithImageSize { imgSz =>
      editor.switchToSelectMode(e, imgSz)
    }
  }

  @FXML
  def addCropModeClicked(e: ActionEvent) {
    println("addCropModeClicked")
    doWithImageSize { imgSz =>
      editor.switchToAddCropMode(e, imgSz)
    }
  }

  lazy val absoluteFieldDialog: (SfxAlert, AbsoluteFieldController) = {
    val loader = new FXMLLoader(getClass().getResource("tesseract.fxml"))
    val root: DialogPane = loader.load()
    val ctrl = loader.getController().asInstanceOf[AbsoluteFieldController]
    val alert = new SfxAlert(AlertType.Confirmation)
    alert.dialogPane = new SfxDialogPane(root)
    alert.title = "フィールド設定"
    (alert, ctrl)
  }

  @FXML
  def canvasMouseClicked(e: MouseEvent) {
    println("canvasMouseClicked")
    if (e.getClickCount == 2) {
      println("double clicked")
      doWithImageSize { imgSz =>
        project.getSelectedAbsoluteFieldAt(imgSz, e.getX, e.getY) foreach { f =>
          val (dlg, ctrl) = absoluteFieldDialog
          ctrl.fieldName = f.name
          dlg.showAndWait().map(_.delegate) match {
            case Some(ButtonType.APPLY) =>
              project.renameSelectedAbsoluteField(f, ctrl.fieldName)
            case Some(_) =>
              println("canceled")
            case None =>
              println("bt = none")
          }

          // val dlg = new SfxTextInputDialog(f.name)
          // dlg.title = "フィールドの名前変更"
          // dlg.headerText = "フィールドの名前を入力してください。"
          // dlg.showAndWait() foreach { newName =>
          //   project.renameSelectedAbsoluteField(f, newName)
          // }
        }
      }
    }
  }

  @FXML
  def canvasMousePressed(e: MouseEvent) {
    println("canvasMousePressed")
    doWithImageSize { imgSz =>
      editor.onMousePressed(e, imgSz)
    }
  }

  @FXML
  def canvasMouseDragged(e: MouseEvent) {
    println("canvasMouseDragged")
    doWithImageSize { imgSz =>
      editor.onMouseDragged(e, imgSz)
    }
  }

  @FXML
  def canvasMouseReleased(e: MouseEvent) {
    println("canvasMouseReleased")
    doWithImageSize { imgSz =>
      editor.onMouseReleased(e, imgSz)
    }
  }

  @FXML
  def canvasMouseEntered(e: MouseEvent) {
    doWithImageSize { imgSz =>
      editor.onMouseEntered(e, imgSz)
    }
  }

  @FXML
  def canvasMouseExited(e: MouseEvent) {
    doWithImageSize { imgSz =>
      editor.onMouseExited(e, imgSz)
    }
  }

  @FXML
  def canvasMouseMoved(e: MouseEvent) {
    doWithImageSize { imgSz =>
      editor.onMouseMoved(e, imgSz)
    }
  }

  @FXML
  def runCaptureClicked(e: ActionEvent) {
    println("runCapture")

    selectedImage.foreach { si =>
      doBigJob(Right(project.runCapture(si))) {
        case Right(resp) =>
          val alert = new Alert(AlertType.Information)
          alert.setTitle("処理結果")
          alert.setContentText(
            resp.serverResp.map { e =>
              e.fieldName + ": " + e.rawText
            }.mkString("\r\n")
          )
          alert.showAndWait()
        case Left(fail) =>
          fail match {
            case e: RestAuthFailure =>
              val alert = new Alert(AlertType.Error)
              alert.setTitle("認証失敗")
              alert.setContentText(
                "認証に失敗しました。認証の設定を確認してください。"
              )
              alert.showAndWait()
            case e: RestUnknownFailure =>
              val alert = new Alert(AlertType.Error)
              alert.setTitle("サーバ通信エラー")
              alert.setContentText(
                "サーバとの通信に失敗しました。"
              )
              alert.showAndWait()
          }
      }
    }
  }

  @FXML
  def authSettingsMenuClicked(e: ActionEvent) {
    println("authSettingsMenuClicked")
    val loader = new FXMLLoader(getClass().getResource("auth.fxml"))
    val root: DialogPane = loader.load()
    val ctrl = loader.getController().asInstanceOf[AuthController]
    val settings: Settings = settingsLoader.settings
    ctrl.model = settings.auth
    val alert = new SfxAlert(AlertType.Confirmation)
    alert.dialogPane = new SfxDialogPane(root)
    alert.title = "認証設定"
    alert.showAndWait().map(_.delegate) match {
      case Some(ButtonType.APPLY) =>
        val newAuth: AuthSettings = ctrl.model
        settingsLoader.update(settings.copy(auth = newAuth))
      case Some(_) => println("canceled")
      case None => println("bt = none")
    }
  }

  @FXML
  def helpAboutClicked(e: ActionEvent) {
    println("helpAboutClicked")

    val alert = new SfxAlert(AlertType.None) {
      title = "About"
      contentText = "FormBuilder " + Version.parse(generated.BuildInfo.version)
      buttonTypes = Seq(SfxButtonType.OK)
    }
    alert.showAndWait()
  }

  override def initialize(url: URL, resourceBundle: ResourceBundle) {
    imageTable.addChangeListener(new ImageTableListener() {
      override def onFilesChanged(before: imm.Set[File], after: imm.Set[File]) {
        sfxImageListView.items = ObservableBuffer(after.toSeq)
      }
    });

    addModeButton.setSelected(true)

    project = ProjectImpl(
      new ProjectContext(
        onNormalAbsoluteFieldAdded = (f: AbsoluteField) => {
          println("*** normal field added " + f)
          doWithImageSize { imgSz =>
            f.draw(imgSz, sfxImageCanvas.graphicsContext2D, false)
          }
        },
        onSelectedAbsoluteFieldAdded = (f: AbsoluteField) => {
          println("*** selected field added " + f)
          doWithImageSize { imgSz =>
            f.draw(imgSz, sfxImageCanvas.graphicsContext2D, true)
          }
        },
        onNormalAbsoluteFieldRemoved = (f: AbsoluteField) => {
          println("*** normal field removed " + f)
          doWithImageSize { imgSz =>
            redrawRect(f.drawArea(imgSz))
          }
        },
        onSelectedAbsoluteFieldRemoved = (f: AbsoluteField) => {
          println("*** selected field removed " + f)
          doWithImageSize { imgSz =>
            redrawRect(f.drawArea(imgSz))
          }
        },
        onNormalCropFieldAdded = (f: CropField) => {
          println("onNormalCropFieldAdded")
          project.invalidateCachedImage(CacheConditionGlob(isCropEnabled = Some(true)))
          doWithImageSize { imgSz =>
            f.draw(imgSz, sfxImageCanvas.graphicsContext2D, false)
          }
        },
        onSelectedCropFieldAdded = (f: CropField) => {
          println("onSelectedCropFieldAdded")
          project.invalidateCachedImage(CacheConditionGlob(isCropEnabled = Some(true)))
          doWithImageSize { imgSz =>
            f.draw(imgSz, sfxImageCanvas.graphicsContext2D, true)
          }
        },
        onNormalCropFieldRemoved = (f: CropField) => {
          println("onNormalCropFieldRemoved")
          project.invalidateCachedImage(CacheConditionGlob(isCropEnabled = Some(true)))
          doWithImageSize { imgSz =>
            redrawRect(f.drawArea(imgSz))
          }
        },
        onSelectedCropFieldRemoved = (f: CropField) => {
          println("onSelectedCropFieldRemoved")
          project.invalidateCachedImage(CacheConditionGlob(isCropEnabled = Some(true)))
          doWithImageSize { imgSz =>
            redrawRect(f.drawArea(imgSz))
          }
        },
        redrawCropField = (f: CropField) => {
          doWithImageSize { imgSz =>
            redrawRect(f.drawArea(imgSz))
          }
        },
        selectCropField = (f: CropField, selected: Boolean) => {
          doWithImageSize { imgSz =>
            redrawRect(f.drawArea(imgSz))
          }
        }
      ),
      new ProjectListener {
        override def onSkewCorrectionChanged(skewCorrection: SkewCorrection) {
          println("Skew correction changed " + skewCorrection)
          sfxSkewCorrectionCheck.selected = skewCorrection.enabled
        }

        override def onCropEnabledChanged(enabled: Boolean) {
          println("crop enabled changed " + enabled)
          sfxCropCheck.selected = enabled
        }

        override def onDotRemovalChanged(dotRemoval: DotRemoval): Unit = {
          println("dot removal changed " + dotRemoval)
          sfxDotRemovalCheck.selected = dotRemoval.enabled
        }

        override def onCropRectangleChanged(cropRectangle: CropRectangle): Unit = {
          println("crop rectangle changed " + cropRectangle)
          sfxCropRectangleCheck.selected = cropRectangle.enabled
        }

        override def onRemoveRuledLineChanged(removeRuledLine: RemoveRuledLine): Unit = {
          println("remove ruled line changed " + removeRuledLine)
          sfxRemoveRuledLineCheck.selected = removeRuledLine.enabled
        }
      }
    )

    editor = new Editor(
      project,
      EditorContext(
        drawWidget, redrawRect, fieldCreated, drawSelectionRect, selectFields, setMouseCursor
      )
    )
    editor.initialize()

    performUpdateCheck()
  }

  def performUpdateCheck() {
    val latestVersion: Version = Await.result(
      Ws().url(ModuleServerUrl + "/getLatest.json").withQueryStringParameters(
        "moduleName" -> "formBuilder",
        "baseName" -> "formbuilder"
      ).get().map { resp =>
        val json = Json.parse(resp.body)
        Version.parse(
          (json \ "version").as[String]
        )
      },
      Duration(1, TimeUnit.MINUTES)
    )
    
    println(s"Current version: ${generated.BuildInfo.version}, Latest version: ${latestVersion}")
    if (latestVersion > Version.parse(generated.BuildInfo.version)) {
      val dlg = new SfxAlert(
        alertType = SfxAlert.AlertType.Confirmation
      ) {
        title = "アプリケーション更新"
        contentText = s"アプリケーションの更新が必要です。\nクリックして進んでください。\n現在:${generated.BuildInfo.version}, 最新:${latestVersion}\nwithout JDK版を使用している方は取消を選び、\nご自分で最新版をDownloadしてください。"
        buttonTypes = Seq(
          SfxButtonType.Apply, SfxButtonType.Cancel
        )
      }
      dlg.initOwner(stage)
      dlg.showAndWait() match {
        case Some(SfxButtonType.Apply) => performUpdate(latestVersion)
        case _ => Platform.runLater(terminateApplication())
      }
    }
  }

  def performUpdate(v: Version) {
    implicit val system = ActorSystem()
    implicit val materializer = ActorMaterializer()
    val cwd = Paths.get(System.getProperty("user.dir"))
    val fileName = Os().fileName(v)
    val downloadedFile = cwd.resolve(fileName)
    println("Downloading + " + downloadedFile)
    val dlg = new Alert(AlertType.None)
    dlg.setTitle("ダウンロード中")
    dlg.setContentText("ダウンロード中。お待ちください")
    dlg.initOwner(stage)

    Ws().url(ModuleServerUrl + "/getModule").withQueryStringParameters(
      "moduleName" -> "formBuilder",
      "fileName" -> fileName
    ).get().map { resp =>
      resp.status match {
        case 200 =>
          val m = resp.bodyAsSource.runWith(FileIO.toPath(downloadedFile))
          m.onComplete { _ =>
            println("Download completed.")
            println("Explode " + downloadedFile + " into " + cwd)
            Platform.runLater(terminateApplication())
          }
        case _ =>
          println("Cannot download file status: " + resp.status + "(" + resp.statusText + ")")
          dlg.getDialogPane().getButtonTypes().addAll(ButtonType.CANCEL);
          dlg.close()
          Platform.runLater {
            val dlg = new SfxAlert(AlertType.Error) {
              title = "ダウンロード エラー"
              contentText = "ダウンロードに失敗しました。code: " + resp.status + "\n(" + resp.statusText + ")"
            }
            dlg.showAndWait()
            Platform.runLater(terminateApplication())
          }
      }
    }

    dlg.showAndWait()
  }
}
