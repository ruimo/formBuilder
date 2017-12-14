package com.ruimo.forms

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
import java.nio.file._
import java.util
import java.util.ResourceBundle
import java.util.concurrent.TimeUnit
import java.util.zip.{ZipEntry, ZipOutputStream}
import javafx.scene.{Cursor, Scene}

import scalafx.scene.control.{Alert => SfxAlert, ListView => SfxListView, TextInputDialog => SfxTextInputDialog}
import scalafx.scene.control.{ChoiceDialog => SfxChoiceDialog}
import scalafx.scene.control.{CheckBox => SfxCheckBox}
import javafx.scene.canvas.Canvas
import javafx.scene.control._
import javafx.scene.paint.Color
import javafx.stage.WindowEvent

import scalafx.scene.canvas.{Canvas => SfxCanvas}
import scalafx.scene.control.{DialogPane => SfxDialogPane}
import scalafx.scene.control.Alert.AlertType
import play.api.libs.ws.JsonBodyReadables._
import play.api.libs.ws.JsonBodyWritables._

import scala.concurrent.ExecutionContext.Implicits.global
import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import com.ruimo.scoins.{LoanPattern, PathUtil, Zip}
import com.ruimo.scoins.LoanPattern._
import play.api.libs.json._

import scalafx.geometry.{Point2D, Rectangle2D}
import Helpers.toRect

import scala.collection.immutable.Seq
import scala.concurrent.duration.Duration

case class EditorContext(
  drawWidget: (Widget[_], Boolean) => Unit,
  redrawRect: (Rectangle2D) => Unit,
  fieldCreated: (Field) => Unit,
  drawSelectionRect: (Rectangle2D) => Unit,
  selectFields: (Rectangle2D, MouseEvent) => Unit,
  setMouseCursor: Cursor => Unit,
  formSize: () => (Double, Double)
)

object ModeEditors {
  sealed trait ModeEditor {
    def onMousePressed(e: MouseEvent): ModeEditor
    def onMouseDragged(e: MouseEvent): ModeEditor
    def onMouseReleased(e: MouseEvent): ModeEditor
    def onMouseEntered(e: MouseEvent): ModeEditor
    def onMouseExited(e: MouseEvent): ModeEditor
    def onMouseMoved(e: MouseEvent): ModeEditor
    def switchToAddMode(e: ActionEvent): ModeEditor
    def switchToAddCropMode(e: ActionEvent): ModeEditor
    def switchToSelectMode(e: ActionEvent): ModeEditor
  }

  object AddCropMode {
    case class Init(
      project: Project,
      editorContext: EditorContext
    ) extends ModeEditor {
      def onMousePressed(e: MouseEvent): ModeEditor = {
        project.deselectAllFields()
        Start(project, editorContext, new Point2D(e.getX, e.getY))
      }
      def onMouseDragged(e: MouseEvent): ModeEditor = this
      def onMouseReleased(e: MouseEvent): ModeEditor = this
      def onMouseEntered(e: MouseEvent): ModeEditor = this
      def onMouseExited(e: MouseEvent): ModeEditor = this
      def onMouseMoved(e: MouseEvent): ModeEditor = this
      def switchToAddMode(e: ActionEvent): ModeEditor = AddMode.Init(project, editorContext)
      def switchToAddCropMode(e: ActionEvent): ModeEditor = this
      def switchToSelectMode(e: ActionEvent): ModeEditor = SelectMode.Init(project, editorContext)
    }

    case class Start(
      project: Project,
      editorContext: EditorContext,
      p0: Point2D
    ) extends ModeEditor {
      def onMousePressed(e: MouseEvent): ModeEditor = this
      def onMouseDragged(e: MouseEvent): ModeEditor = {
        val p1 = new Point2D(e.getX, e.getY)
        val adding = UnknownCropFieldImpl(editorContext.formSize(), toRect(p0, p1))
        editorContext.drawWidget(adding, true)
        Dragging(adding, project, editorContext, p0, p1)
      }
      def onMouseReleased(e: MouseEvent): ModeEditor = Init(project, editorContext)
      def onMouseEntered(e: MouseEvent): ModeEditor = this
      def onMouseExited(e: MouseEvent): ModeEditor = this
      def onMouseMoved(e: MouseEvent): ModeEditor = this
      def switchToAddMode(e: ActionEvent): ModeEditor = AddMode.Init(project, editorContext)
      def switchToAddCropMode(e: ActionEvent): ModeEditor = Init(project, editorContext)
      def switchToSelectMode(e: ActionEvent): ModeEditor = SelectMode.Init(project, editorContext)
    }

    case class Dragging(
      adding: Field,
      project: Project,
      editorContext: EditorContext,
      p0: Point2D, p1: Point2D
    ) extends ModeEditor {
      def onMousePressed(e: MouseEvent): ModeEditor = this
      def onMouseDragged(e: MouseEvent): ModeEditor = {
        val newP1 = new Point2D(e.getX, e.getY)
        editorContext.redrawRect(adding.drawArea(editorContext.formSize()))
        val newAdding = UnknownCropFieldImpl(editorContext.formSize(), toRect(p0, newP1))
        editorContext.drawWidget(newAdding, true)
        Dragging(newAdding, project, editorContext, p0, newP1)
      }
      def onMouseReleased(e: MouseEvent): ModeEditor = {
        val newP1 = new Point2D(e.getX, e.getY)
        editorContext.redrawRect(adding.drawArea(editorContext.formSize()))
        val newAdding = UnknownCropFieldImpl(editorContext.formSize(), toRect(p0, newP1))
        editorContext.drawWidget(newAdding, true)
        editorContext.fieldCreated(newAdding)
        Init(project, editorContext)
      }
      def onMouseEntered(e: MouseEvent): ModeEditor = this
      def onMouseExited(e: MouseEvent): ModeEditor = this
      def onMouseMoved(e: MouseEvent): ModeEditor = this
      def switchToAddMode(e: ActionEvent): ModeEditor = AddMode.Init(project, editorContext)
      def switchToAddCropMode(e: ActionEvent): ModeEditor = Init(project, editorContext)
      def switchToSelectMode(e: ActionEvent): ModeEditor = SelectMode.Init(project, editorContext)
    }
  }

  object AddMode {
    case class Init(
      project: Project,
      editorContext: EditorContext
    ) extends ModeEditor {
      assume(project != null)
      assume(editorContext != null)

      def onMousePressed(e: MouseEvent): ModeEditor = {
        project.deselectAllFields()
        Start(project, editorContext, new Point2D(e.getX, e.getY))
      }
      def onMouseDragged(e: MouseEvent): ModeEditor = this
      def onMouseReleased(e: MouseEvent): ModeEditor = this
      def onMouseEntered(e: MouseEvent): ModeEditor = this
      def onMouseExited(e: MouseEvent): ModeEditor = this
      def onMouseMoved(e: MouseEvent): ModeEditor = this
      def switchToAddMode(e: ActionEvent): ModeEditor = this
      def switchToAddCropMode(e: ActionEvent): ModeEditor = AddCropMode.Init(project, editorContext)
      def switchToSelectMode(e: ActionEvent): ModeEditor = SelectMode.Init(project, editorContext)
    }

    case class Start(
      project: Project,
      editorContext: EditorContext,
      p0: Point2D
    ) extends ModeEditor {
      def onMousePressed(e: MouseEvent): ModeEditor = this
      def onMouseDragged(e: MouseEvent): ModeEditor = {
        val p1 = new Point2D(e.getX, e.getY)
        val adding = AbsoluteFieldImpl(editorContext.formSize(), toRect(p0, p1), "adding")
        editorContext.drawWidget(adding, true)
        Dragging(adding, project, editorContext, p0, p1)
      }
      def onMouseReleased(e: MouseEvent): ModeEditor = Init(project, editorContext)
      def onMouseEntered(e: MouseEvent): ModeEditor = this
      def onMouseExited(e: MouseEvent): ModeEditor = this
      def onMouseMoved(e: MouseEvent): ModeEditor = this
      def switchToAddMode(e: ActionEvent): ModeEditor = AddMode.Init(project, editorContext)
      def switchToAddCropMode(e: ActionEvent): ModeEditor = AddCropMode.Init(project, editorContext)
      def switchToSelectMode(e: ActionEvent): ModeEditor = SelectMode.Init(project, editorContext)
    }

    case class Dragging(
      adding: Field,
      project: Project,
      editorContext: EditorContext,
      p0: Point2D, p1: Point2D
    ) extends ModeEditor {
      def onMousePressed(e: MouseEvent): ModeEditor = this
      def onMouseDragged(e: MouseEvent): ModeEditor = {
        val newP1 = new Point2D(e.getX, e.getY)
        editorContext.redrawRect(adding.drawArea(editorContext.formSize()))
        val newAdding = AbsoluteFieldImpl(editorContext.formSize(), toRect(p0, newP1), "adding")
        editorContext.drawWidget(newAdding, true)
        Dragging(newAdding, project, editorContext, p0, newP1)
      }
      def onMouseReleased(e: MouseEvent): ModeEditor = {
        val newP1 = new Point2D(e.getX, e.getY)
        editorContext.redrawRect(adding.drawArea(editorContext.formSize()))
        val newAdding = AbsoluteFieldImpl(editorContext.formSize(), toRect(p0, newP1), "adding")
        editorContext.drawWidget(newAdding, true)
        editorContext.fieldCreated(newAdding)
        Init(project, editorContext)
      }
      def onMouseEntered(e: MouseEvent): ModeEditor = this
      def onMouseExited(e: MouseEvent): ModeEditor = this
      def onMouseMoved(e: MouseEvent): ModeEditor = this
      def switchToAddMode(e: ActionEvent): ModeEditor = AddMode.Init(project, editorContext)
      def switchToAddCropMode(e: ActionEvent): ModeEditor = AddCropMode.Init(project, editorContext)
      def switchToSelectMode(e: ActionEvent): ModeEditor = SelectMode.Init(project, editorContext)
    }
  }

  object SelectMode {
    case class Init(
      project: Project,
      editorContext: EditorContext
    ) extends ModeEditor {
      def onMousePressed(e: MouseEvent): ModeEditor = {
        def doNext(f: Field): ModeEditor = {
          f.possibleMouseOperation(editorContext.formSize(), e.getX, e.getY) match {
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

        project.getSelectedFieldAt(editorContext.formSize(), e.getX, e.getY) match {
          case Some(f) => doNext(f)
          case None =>
            project.getNormalFieldAt(editorContext.formSize(), e.getX, e.getY) match {
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
      def onMouseDragged(e: MouseEvent): ModeEditor = this
      def onMouseReleased(e: MouseEvent): ModeEditor = this
      def onMouseEntered(e: MouseEvent): ModeEditor = onMouseMoved(e)
      def onMouseExited(e: MouseEvent): ModeEditor = {
        editorContext.setMouseCursor(Cursor.DEFAULT)
        this
      }
      def onMouseMoved(e: MouseEvent): ModeEditor = {
        project.possibleMouseOperation(editorContext.formSize(), e.getX, e.getY) match {
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
      def switchToAddMode(e: ActionEvent): ModeEditor = AddMode.Init(project, editorContext)
      def switchToAddCropMode(e: ActionEvent): ModeEditor = AddCropMode.Init(project, editorContext)
      def switchToSelectMode(e: ActionEvent): ModeEditor = this
    }

    case class Start(
      project: Project,
      editorContext: EditorContext,
      p0: Point2D
    ) extends ModeEditor {
      def onMousePressed(e: MouseEvent): ModeEditor = this
      def onMouseDragged(e: MouseEvent): ModeEditor = {
        val p1 = new Point2D(e.getX, e.getY)
        val sw = SelectionWidget(toRect(p0, p1))
        editorContext.drawWidget(sw, false)
        Dragging(sw, project, editorContext, p0, p1)
      }
      def onMouseReleased(e: MouseEvent): ModeEditor = {
        project.selectSingleFieldAt(editorContext.formSize(), e.getX, e.getY)
        editorContext.setMouseCursor(Cursor.DEFAULT)
        Init(project, editorContext)
      }
      def onMouseEntered(e: MouseEvent): ModeEditor = this
      def onMouseExited(e: MouseEvent): ModeEditor = {
        editorContext.setMouseCursor(Cursor.DEFAULT)
        this
      }
      def onMouseMoved(e: MouseEvent): ModeEditor = this
      def switchToAddMode(e: ActionEvent): ModeEditor = AddMode.Init(project, editorContext)
      def switchToAddCropMode(e: ActionEvent): ModeEditor = AddCropMode.Init(project, editorContext)
      def switchToSelectMode(e: ActionEvent): ModeEditor = SelectMode.Init(project, editorContext)
    }

    case class Moving(
      project: Project,
      editorContext: EditorContext,
      p0: Point2D
    ) extends ModeEditor {
      def onMousePressed(e: MouseEvent): ModeEditor = this
      def onMouseDragged(e: MouseEvent): ModeEditor = {
        val p1 = new Point2D(e.getX, e.getY)
        project.moveSelectedFields(editorContext.formSize(), p0, p1)
        Moving(project, editorContext, p1)
      }
      def onMouseReleased(e: MouseEvent): ModeEditor = {
        editorContext.setMouseCursor(Cursor.DEFAULT)
        Init(project, editorContext)
      }
      def onMouseEntered(e: MouseEvent): ModeEditor = this
      def onMouseExited(e: MouseEvent): ModeEditor = this
      def onMouseMoved(e: MouseEvent): ModeEditor = this
      def switchToAddMode(e: ActionEvent): ModeEditor = AddMode.Init(project, editorContext)
      def switchToAddCropMode(e: ActionEvent): ModeEditor = AddCropMode.Init(project, editorContext)
      def switchToSelectMode(e: ActionEvent): ModeEditor = SelectMode.Init(project, editorContext)
    }

    case class NorthResize(
      project: Project,
      editorContext: EditorContext,
      p0: Point2D
    ) extends ModeEditor {
      def onMousePressed(e: MouseEvent): ModeEditor = this
      def onMouseDragged(e: MouseEvent): ModeEditor = {
        val p1 = new Point2D(e.getX, e.getY)
        project.northResizeSelectedFields(editorContext.formSize(), p0, p1)
        NorthResize(project, editorContext, p1)
      }
      def onMouseReleased(e: MouseEvent): ModeEditor = {
        editorContext.setMouseCursor(Cursor.DEFAULT)
        Init(project, editorContext)
      }
      def onMouseEntered(e: MouseEvent): ModeEditor = this
      def onMouseExited(e: MouseEvent): ModeEditor = this
      def onMouseMoved(e: MouseEvent): ModeEditor = this
      def switchToAddMode(e: ActionEvent): ModeEditor = AddMode.Init(project, editorContext)
      def switchToAddCropMode(e: ActionEvent): ModeEditor = AddCropMode.Init(project, editorContext)
      def switchToSelectMode(e: ActionEvent): ModeEditor = SelectMode.Init(project, editorContext)
    }

    case class EastResize(
      project: Project,
      editorContext: EditorContext,
      p0: Point2D
    ) extends ModeEditor {
      def onMousePressed(e: MouseEvent): ModeEditor = this
      def onMouseDragged(e: MouseEvent): ModeEditor = {
        val p1 = new Point2D(e.getX, e.getY)
        project.eastResizeSelectedFields(editorContext.formSize(), p0, p1)
        EastResize(project, editorContext, p1)
      }
      def onMouseReleased(e: MouseEvent): ModeEditor = {
        editorContext.setMouseCursor(Cursor.DEFAULT)
        Init(project, editorContext)
      }
      def onMouseEntered(e: MouseEvent): ModeEditor = this
      def onMouseExited(e: MouseEvent): ModeEditor = this
      def onMouseMoved(e: MouseEvent): ModeEditor = this
      def switchToAddMode(e: ActionEvent): ModeEditor = AddMode.Init(project, editorContext)
      def switchToAddCropMode(e: ActionEvent): ModeEditor = AddCropMode.Init(project, editorContext)
      def switchToSelectMode(e: ActionEvent): ModeEditor = SelectMode.Init(project, editorContext)
    }

    case class WestResize(
      project: Project,
      editorContext: EditorContext,
      p0: Point2D
    ) extends ModeEditor {
      def onMousePressed(e: MouseEvent): ModeEditor = this
      def onMouseDragged(e: MouseEvent): ModeEditor = {
        val p1 = new Point2D(e.getX, e.getY)
        project.westResizeSelectedFields(editorContext.formSize(), p0, p1)
        WestResize(project, editorContext, p1)
      }
      def onMouseReleased(e: MouseEvent): ModeEditor = {
        editorContext.setMouseCursor(Cursor.DEFAULT)
        Init(project, editorContext)
      }
      def onMouseEntered(e: MouseEvent): ModeEditor = this
      def onMouseExited(e: MouseEvent): ModeEditor = this
      def onMouseMoved(e: MouseEvent): ModeEditor = this
      def switchToAddMode(e: ActionEvent): ModeEditor = AddMode.Init(project, editorContext)
      def switchToAddCropMode(e: ActionEvent): ModeEditor = AddCropMode.Init(project, editorContext)
      def switchToSelectMode(e: ActionEvent): ModeEditor = SelectMode.Init(project, editorContext)
    }

    case class SouthResize(
      project: Project,
      editorContext: EditorContext,
      p0: Point2D
    ) extends ModeEditor {
      def onMousePressed(e: MouseEvent): ModeEditor = this
      def onMouseDragged(e: MouseEvent): ModeEditor = {
        val p1 = new Point2D(e.getX, e.getY)
        project.southResizeSelectedFields(editorContext.formSize(), p0, p1)
        SouthResize(project, editorContext, p1)
      }
      def onMouseReleased(e: MouseEvent): ModeEditor = {
        editorContext.setMouseCursor(Cursor.DEFAULT)
        Init(project, editorContext)
      }
      def onMouseEntered(e: MouseEvent): ModeEditor = this
      def onMouseExited(e: MouseEvent): ModeEditor = this
      def onMouseMoved(e: MouseEvent): ModeEditor = this
      def switchToAddMode(e: ActionEvent): ModeEditor = AddMode.Init(project, editorContext)
      def switchToAddCropMode(e: ActionEvent): ModeEditor = AddCropMode.Init(project, editorContext)
      def switchToSelectMode(e: ActionEvent): ModeEditor = SelectMode.Init(project, editorContext)
    }

    case class NorthWestResize(
      project: Project,
      editorContext: EditorContext,
      p0: Point2D
    ) extends ModeEditor {
      def onMousePressed(e: MouseEvent): ModeEditor = this
      def onMouseDragged(e: MouseEvent): ModeEditor = {
        val p1 = new Point2D(e.getX, e.getY)
        project.northWestResizeSelectedFields(editorContext.formSize(), p0, p1)
        NorthWestResize(project, editorContext, p1)
      }
      def onMouseReleased(e: MouseEvent): ModeEditor = {
        editorContext.setMouseCursor(Cursor.DEFAULT)
        Init(project, editorContext)
      }
      def onMouseEntered(e: MouseEvent): ModeEditor = this
      def onMouseExited(e: MouseEvent): ModeEditor = this
      def onMouseMoved(e: MouseEvent): ModeEditor = this
      def switchToAddMode(e: ActionEvent): ModeEditor = AddMode.Init(project, editorContext)
      def switchToAddCropMode(e: ActionEvent): ModeEditor = AddCropMode.Init(project, editorContext)
      def switchToSelectMode(e: ActionEvent): ModeEditor = SelectMode.Init(project, editorContext)
    }

    case class NorthEastResize(
      project: Project,
      editorContext: EditorContext,
      p0: Point2D
    ) extends ModeEditor {
      def onMousePressed(e: MouseEvent): ModeEditor = this
      def onMouseDragged(e: MouseEvent): ModeEditor = {
        val p1 = new Point2D(e.getX, e.getY)
        project.northEastResizeSelectedFields(editorContext.formSize(), p0, p1)
        NorthEastResize(project, editorContext, p1)
      }
      def onMouseReleased(e: MouseEvent): ModeEditor = {
        editorContext.setMouseCursor(Cursor.DEFAULT)
        Init(project, editorContext)
      }
      def onMouseEntered(e: MouseEvent): ModeEditor = this
      def onMouseExited(e: MouseEvent): ModeEditor = this
      def onMouseMoved(e: MouseEvent): ModeEditor = this
      def switchToAddMode(e: ActionEvent): ModeEditor = AddMode.Init(project, editorContext)
      def switchToAddCropMode(e: ActionEvent): ModeEditor = AddCropMode.Init(project, editorContext)
      def switchToSelectMode(e: ActionEvent): ModeEditor = SelectMode.Init(project, editorContext)
    }

    case class SouthWestResize(
      project: Project,
      editorContext: EditorContext,
      p0: Point2D
    ) extends ModeEditor {
      def onMousePressed(e: MouseEvent): ModeEditor = this
      def onMouseDragged(e: MouseEvent): ModeEditor = {
        val p1 = new Point2D(e.getX, e.getY)
        project.southWestResizeSelectedFields(editorContext.formSize(), p0, p1)
        SouthWestResize(project, editorContext, p1)
      }
      def onMouseReleased(e: MouseEvent): ModeEditor = {
        editorContext.setMouseCursor(Cursor.DEFAULT)
        Init(project, editorContext)
      }
      def onMouseEntered(e: MouseEvent): ModeEditor = this
      def onMouseExited(e: MouseEvent): ModeEditor = this
      def onMouseMoved(e: MouseEvent): ModeEditor = this
      def switchToAddMode(e: ActionEvent): ModeEditor = AddMode.Init(project, editorContext)
      def switchToAddCropMode(e: ActionEvent): ModeEditor = AddCropMode.Init(project, editorContext)
      def switchToSelectMode(e: ActionEvent): ModeEditor = SelectMode.Init(project, editorContext)
    }

    case class SouthEastResize(
      project: Project,
      editorContext: EditorContext,
      p0: Point2D
    ) extends ModeEditor {
      def onMousePressed(e: MouseEvent): ModeEditor = this
      def onMouseDragged(e: MouseEvent): ModeEditor = {
        val p1 = new Point2D(e.getX, e.getY)
        project.southEastResizeSelectedFields(editorContext.formSize(), p0, p1)
        SouthEastResize(project, editorContext, p1)
      }
      def onMouseReleased(e: MouseEvent): ModeEditor = {
        editorContext.setMouseCursor(Cursor.DEFAULT)
        Init(project, editorContext)
      }
      def onMouseEntered(e: MouseEvent): ModeEditor = this
      def onMouseExited(e: MouseEvent): ModeEditor = this
      def onMouseMoved(e: MouseEvent): ModeEditor = this
      def switchToAddMode(e: ActionEvent): ModeEditor = AddMode.Init(project, editorContext)
      def switchToAddCropMode(e: ActionEvent): ModeEditor = AddCropMode.Init(project, editorContext)
      def switchToSelectMode(e: ActionEvent): ModeEditor = SelectMode.Init(project, editorContext)
    }

    case class Dragging(
      sw: SelectionWidget,
      project: Project,
      editorContext: EditorContext,
      p0: Point2D, p1: Point2D
    ) extends ModeEditor {
      def onMousePressed(e: MouseEvent): ModeEditor = this
      def onMouseDragged(e: MouseEvent): ModeEditor = {
        val newP1 = new Point2D(e.getX, e.getY)
        val newSw = SelectionWidget(toRect(p0, newP1))

        editorContext.redrawRect(sw.drawArea(editorContext.formSize()))
        editorContext.drawWidget(newSw, false)
        Dragging(newSw, project, editorContext, p0, newP1)
      }
      def onMouseReleased(e: MouseEvent): ModeEditor = {
        editorContext.setMouseCursor(Cursor.DEFAULT)
        val newP1 = new Point2D(e.getX, e.getY)
        editorContext.redrawRect(sw.drawArea(editorContext.formSize()))
        editorContext.selectFields(toRect(p0, newP1), e)
        Init(project, editorContext)
      }
      def onMouseEntered(e: MouseEvent): ModeEditor = this
      def onMouseExited(e: MouseEvent): ModeEditor = this
      def onMouseMoved(e: MouseEvent): ModeEditor = this
      def switchToAddMode(e: ActionEvent): ModeEditor = AddMode.Init(project, editorContext)
      def switchToAddCropMode(e: ActionEvent): ModeEditor = AddCropMode.Init(project, editorContext)
      def switchToSelectMode(e: ActionEvent): ModeEditor = SelectMode.Init(project, editorContext)
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

  def onMousePressed(e: MouseEvent) {
    editor = editor.onMousePressed(e)
  }

  def onMouseDragged(e: MouseEvent): Unit = {
    editor = editor.onMouseDragged(e)
  }

  def onMouseReleased(e: MouseEvent): Unit = {
    editor = editor.onMouseReleased(e)
  }

  def onMouseExited(e: MouseEvent): Unit = {
    editor = editor.onMouseExited(e)
  }

  def onMouseEntered(e: MouseEvent): Unit = {
    editor = editor.onMouseEntered(e)
  }

  def onMouseMoved(e: MouseEvent): Unit = {
    editor = editor.onMouseMoved(e)
  }

  def switchToAddMode(e: ActionEvent): Unit = {
    editor = editor.switchToAddMode(e)
  }

  def switchToSelectMode(e: ActionEvent): Unit = {
    editor = editor.switchToSelectMode(e)
  }

  def switchToAddCropMode(e: ActionEvent): Unit = {
    editor = editor.switchToAddCropMode(e)
  }
}

case class SelectedImage(file: Path, image: Image) {
  val imageSize: (Double, Double) = (image.getWidth, image.getHeight)
}

class MainController extends Initializable with HandleBigJob {
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
            println("DELETE hit")
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
            case Some(SfxButtonType.No) =>
            case _ =>
              e.consume()
          }
        }
      }
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
        case authFail: RestAuthFailure =>
          authError()
        case serverFail: RestUnknownFailure =>
          showGeneralError()
      }
    }
    if (! project.cropEnabled) {
      project.leftCropField.foreach { cf =>
        if (cf.intersects(imageSize, rect)) {
          cf.draw(imageSize, gc, project.isLeftCropFieldSelected)
        }
      }
      project.topCropField.foreach { cf =>
        if (cf.intersects(imageSize, rect)) {
          cf.draw(imageSize, gc, project.isLeftCropFieldSelected)
        }
      }
      project.rightCropField.foreach { cf =>
        if (cf.intersects(imageSize, rect)) {
          cf.draw(imageSize, gc, project.isRightCropFieldSelected)
        }
      }
      project.bottomCropField.foreach { cf =>
        if (cf.intersects(imageSize, rect)) {
          cf.draw(imageSize, gc, project.isBottomCropFieldSelected)
        }
      }
    }
    project.absoluteFields.normalFields.foreach { af =>
      if (af.intersects(imageSize, rect)) {
        af.draw(imageSize, gc, false)
      }
    }
    project.absoluteFields.selectedFields.foreach { af =>
      if (af.intersects(imageSize, rect)) {
        af.draw(imageSize, gc, true)
      }
    }
  }

  def drawWidget(widget: Widget[_], isSelected: Boolean): Unit = {
    widget.draw(imageSize, sfxImageCanvas.graphicsContext2D, isSelected)
  }

  def imageSize: (Double, Double) = selectedImage.get.imageSize

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

    field match {
      case af: AbsoluteField =>
        val dlg = new SfxTextInputDialog("")
        dlg.title = "フィールドの名前入力"
        dlg.headerText = "フィールドの名前を入力してください。"
        dlg.showAndWait() match {
          case None =>
            redrawRect(field.drawArea(imageSize))
          case Some(name) =>
            project.addAbsoluteField(af.withName(name), true)
        }
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
            redrawRect(field.drawArea(imageSize))
          case Some(loc) => (loc match {
            case Left => project.addLeftCropField(cf.toLeft, true)
            case Right => project.addRightCropField(cf.toRight, true)
            case Top => project.addTopCropField(cf.toTop, true)
            case Bottom => project.addBottomCropField(cf.toBottom, true)
          }).foreach { oldCropField =>
            redrawRect(oldCropField.drawArea(imageSize))
          }
        }
    }
  }

  def selectFields(rect: Rectangle2D, e: MouseEvent): Unit = {
    println("selectFields(" + rect + ", " + e + ")")
    project.selectAbsoluteFields(imageSize, rect, e)
    project.selectCropFields(imageSize, rect, e)
  }

  @FXML
  def exitMenuClicked(event: ActionEvent) {
    println("exitMenuClicked()")
    Main.terminate()
  }

  @FXML
  def saveMenuClicked(event: ActionEvent) {
    println("saveMenuClicked()")
    saveProject()
  }

  @FXML
  def imageOpenMenuClicked(event: ActionEvent) {
    val fc = new FileChooser {
      title = "Select image file"
      extensionFilters.add(new FileChooser.ExtensionFilter("Image Files", Seq("*.png", "*.tif", "*.pdf")))
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
                Ws().url(urlPath).addHttpHeaders(
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
          authError()
        case serverFail: RestUnknownFailure =>
          showGeneralError()
      }
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
        project.invalidateCachedImage(true, true)
        project.invalidateCachedImage(true, false)
        if (sfxSkewCorrectionCheck.selected()) {
          sfxSkewCorrectionCheck.selected = false
          skewCorrectionEnabledClicked(null)
          sfxSkewCorrectionCheck.selected = true
          skewCorrectionEnabledClicked(null)
        }

      case Some(_) => println("canceled")
      case None => println("bt = none")
    }

//    val scene: Scene = new Scene(root)
//    val myStage = new Stage()
//    myStage.setTitle("傾き補正")
//    myStage.initModality(Modality.ApplicationModal)
//    myStage.initOwner(stage.getOwner())
//    myStage.setScene(scene)
//    myStage.show()
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
            project.cachedImage(si, isSkewCorrectionEnabled = true),
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
    editor.switchToAddMode(e)
  }

  @FXML
  def selectModeClicked(e: ActionEvent) {
    println("selectModeClicked")
    editor.switchToSelectMode(e)
  }

  @FXML
  def addCropModeClicked(e: ActionEvent) {
    println("addCropModeClicked")
    editor.switchToAddCropMode(e)
  }

  @FXML
  def canvasMouseClicked(e: MouseEvent) {
    println("canvasMouseClicked")
    if (e.getClickCount == 2) {
      println("double clicked")
      project.getSelectedAbsoluteFieldAt(imageSize, e.getX, e.getY) foreach { f =>
        val dlg = new SfxTextInputDialog(f.name)
        dlg.title = "フィールドの名前変更"
        dlg.headerText = "フィールドの名前を入力してください。"
        dlg.showAndWait() foreach { newName =>
          project.renameSelectedAbsoluteField(f, newName)
        }
      }
    }
  }

  @FXML
  def canvasMousePressed(e: MouseEvent) {
    println("canvasMousePressed")
    editor.onMousePressed(e)
  }

  @FXML
  def canvasMouseDragged(e: MouseEvent) {
    println("canvasMouseDragged")
    editor.onMouseDragged(e)
  }

  @FXML
  def canvasMouseReleased(e: MouseEvent) {
    println("canvasMouseReleased")
    editor.onMouseReleased(e)
  }

  @FXML
  def canvasMouseEntered(e: MouseEvent) {
    editor.onMouseEntered(e)
  }

  @FXML
  def canvasMouseExited(e: MouseEvent) {
    editor.onMouseExited(e)
  }

  @FXML
  def canvasMouseMoved(e: MouseEvent) {
    editor.onMouseMoved(e)
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
          f.draw(imageSize, sfxImageCanvas.graphicsContext2D, false)
        },
        onSelectedAbsoluteFieldAdded = (f: AbsoluteField) => {
          println("*** selected field added " + f)
          f.draw(imageSize, sfxImageCanvas.graphicsContext2D, true)
        },
        onNormalAbsoluteFieldRemoved = (f: AbsoluteField) => {
          println("*** normal field removed " + f)
          redrawRect(f.drawArea(imageSize))
        },
        onSelectedAbsoluteFieldRemoved = (f: AbsoluteField) => {
          println("*** selected field removed " + f)
          redrawRect(f.drawArea(imageSize))
        },
        onNormalCropFieldAdded = (f: CropField) => {
          project.invalidateCachedImage(cropped = true)
          f.draw(imageSize, sfxImageCanvas.graphicsContext2D, false)
        },
        onSelectedCropFieldAdded = (f: CropField) => {
          project.invalidateCachedImage(cropped = true)
          f.draw(imageSize, sfxImageCanvas.graphicsContext2D, true)
        },
        onNormalCropFieldRemoved = (f: CropField) => {
          project.invalidateCachedImage(cropped = true)
          redrawRect(f.drawArea(imageSize))
        },
        onSelectedCropFieldRemoved = (f: CropField) => {
          project.invalidateCachedImage(cropped = true)
          redrawRect(f.drawArea(imageSize))
        },
        redrawCropField = (f: CropField) => {
          redrawRect(f.drawArea(imageSize))
        },
        selectCropField = (f: CropField, selected: Boolean) => {
          redrawRect(f.drawArea(imageSize))
        }
      ),
      new ProjectListener {
        def onSkewCorrectionChanged(skewCorrection: SkewCorrection) {
          println("Skew correction changed " + skewCorrection)
          sfxSkewCorrectionCheck.selected = skewCorrection.enabled
        }

        def onCropEnabledChanged(enabled: Boolean) {
          println("crop enabled changed " + enabled)
          sfxCropCheck.selected = enabled
        }
      }
    )

    editor = new Editor(
      project,
      EditorContext(
        drawWidget, redrawRect, fieldCreated, drawSelectionRect, selectFields, setMouseCursor, () => imageSize
      )
    )
    editor.initialize()
  }
}
