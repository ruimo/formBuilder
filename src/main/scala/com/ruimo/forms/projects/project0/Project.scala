package com.ruimo.forms.projects.project0

import scala.concurrent.duration._
import scalafx.application.Platform
import play.api.libs.functional.syntax._

import scala.concurrent.ExecutionContext.Implicits.global
import java.io.FileInputStream
import java.nio.channels.FileChannel
import java.nio.file.{Files, Path, StandardOpenOption}
import java.time.Instant
import java.util.concurrent.TimeUnit
import java.util.zip.ZipInputStream
import javafx.scene.input.MouseEvent
import javafx.scene.paint.Color

import play.api.libs.ws.DefaultBodyWritables._
import play.api.libs.ws.JsonBodyWritables._
import play.api.libs.ws._
import com.ruimo.forms._
import play.api.libs.json._

import scala.collection.{immutable => imm}
import scalafx.geometry.{Point2D, Rectangle2D}
import scalafx.scene.canvas.{GraphicsContext => SfxGraphicsContext}
import com.ruimo.graphics.twodim.Area
import com.ruimo.scoins.{PathUtil, Percent, Zip}

import scalafx.scene.image.Image
import com.ruimo.scoins.LoanPattern._

import scala.annotation.tailrec
import scala.concurrent.{Await, Future}
import scala.concurrent.duration.Duration

object AbsoluteFieldImpl {
  val LineWidth = 2.0

  implicit object absoluteFieldFormat extends Format[AbsoluteField] {
    override def reads(jv: JsValue): JsResult[AbsoluteField] = {
      val field = (jv \ "field").as[JsValue]
      val os = (field \ "originalSize").as[Seq[Double]]
      JsSuccess(
        AbsoluteFieldImpl(
          (os(0), os(1)),
          new Rectangle2D(
            (field \ "x").as[Double], (field \ "y").as[Double],
            (field \ "w").as[Double], (field \ "h").as[Double]
          ),
          (jv \ "name").as[String]
        )
      )
    }

    override def writes(f: AbsoluteField): JsValue = Json.obj(
      "originalSize" -> JsArray(Seq(JsNumber(f.originalSize._1), JsNumber(f.originalSize._2))),
      "x" -> f.rect.getMinX(),
      "y" -> f.rect.getMinY(),
      "w" -> f.rect.getWidth(),
      "h" -> f.rect.getHeight(),
      "name" -> f.name
    )
  }
}

case class AbsoluteFieldImpl(originalSize: (Double, Double), rect: Rectangle2D, name: String) extends AbsoluteField {
  import AbsoluteFieldImpl._
  override type R = AbsoluteFieldImpl

  def drawArea(formSize: (Double, Double)) = new Rectangle2D(
    scaleX(formSize, rect.minX) - LineWidth / 2,
    scaleY(formSize, rect.minY) - LineWidth / 2,
    scaleX(formSize, rect.width) + LineWidth,
    scaleY(formSize, rect.height) + LineWidth
  )

  def draw(formSize: (Double, Double), gc: SfxGraphicsContext, isSelected: Boolean) {
    gc.setLineWidth(LineWidth)
    gc.setStroke(if (isSelected) Color.RED else Color.BLUE)
    gc.setLineDashes()
    gc.strokeRect(
      scaleX(formSize, rect.minX),
      scaleY(formSize, rect.minY),
      scaleX(formSize, rect.width),
      scaleY(formSize, rect.height)
    )
  }

  def withNewRect(newRect: Rectangle2D, formSize: (Double, Double)): R = copy(rect = newRect, originalSize = formSize)

  def withName(newName: String): AbsoluteFieldImpl =
    if (newName != name) copy(name = newName) else this

  def possibleMouseOperation(formSize: (Double, Double), x: Double, y: Double): MouseOperation = {
    val cornerSize = LineWidth * 2
    val rminx = scaleX(formSize, rect.minX)
    val rminy = scaleY(formSize, rect.minY)
    val rmaxx = scaleX(formSize, rect.maxX)
    val rmaxy = scaleY(formSize, rect.maxY)

    if (rminx - cornerSize <= x && x <= rminx + cornerSize
      && rminy - cornerSize <= y && y <= rminy + cornerSize) {
      CanNorthWestResize(this)
    }
    else if (rmaxx - cornerSize <= x && x <= rmaxx + cornerSize
      && rminy - cornerSize <= y && y <= rminy + cornerSize) {
      CanNorthEastResize(this)
    }
    else if (rminx - cornerSize <= x && x <= rminx + cornerSize
      && rmaxy - cornerSize <= y && y <= rmaxy + cornerSize) {
      CanSouthWestResize(this)
    }
    else if (rmaxx - cornerSize <= x && x <= rmaxx + cornerSize
      && rmaxy - cornerSize <= y && y <= rmaxy + cornerSize) {
      CanSouthEastResize(this)
    }
    else if (rminx - cornerSize <= x && x <= rminx + cornerSize) {
      CanWestResize(this)
    }
    else if (rmaxx - cornerSize <= x && x <= rmaxx + cornerSize) {
      CanEastResize(this)
    }
    else if (rminy - cornerSize <= y && y <= rminy + cornerSize) {
      CanNorthResize(this)
    }
    else if (rmaxy - cornerSize <= y && y <= rmaxy + cornerSize) {
      CanSouthResize(this)
    }
    else if (rect.contains(x, y)) {
      CanMove(this)
    }
    else CanDoNothing
  }
}

object CropFieldImpl {
  val LineWidth = 2.0
}

abstract class CropFieldImpl(originalSize: (Double, Double), rect: Rectangle2D) extends CropField {
  type R <: CropFieldImpl
  import CropFieldImpl._

  override def draw(formSize: (Double, Double), gc: SfxGraphicsContext, isSelected: Boolean) {
    gc.setLineWidth(LineWidth)
    gc.setStroke(if (isSelected) Color.RED else Color.BLUE)
    gc.setLineDashes()
    gc.strokeRect(
      scaleX(formSize, rect.minX), scaleY(formSize, rect.minY),
      scaleX(formSize, rect.width), scaleY(formSize, rect.height)
    )
  }

  override def drawArea(formSize: (Double, Double)): Rectangle2D = new Rectangle2D(
    scaleX(formSize, rect.minX) - LineWidth / 2,
    scaleY(formSize, rect.minY) - LineWidth / 2,
    scaleX(formSize, rect.width) + LineWidth,
    scaleY(formSize, rect.height) + LineWidth
  )

  override def possibleMouseOperation(formSize: (Double, Double), x: Double, y: Double): MouseOperation = {
    val cornerSize = LineWidth * 2
    val rminx = scaleX(formSize, rect.minX)
    val rmaxx = scaleX(formSize, rect.maxX)
    val rminy = scaleY(formSize, rect.minY)
    val rmaxy = scaleY(formSize, rect.maxY)

    if (rminx - cornerSize <= x && x <= rminx + cornerSize
      && rminy - cornerSize <= y && y <= rminy + cornerSize) {
      CanNorthWestResize(this)
    }
    else if (rmaxx - cornerSize <= x && x <= rmaxx + cornerSize
      && rminy - cornerSize <= y && y <= rminy + cornerSize) {
      CanNorthEastResize(this)
    }
    else if (rminx - cornerSize <= x && x <= rminx + cornerSize
      && rmaxy - cornerSize <= y && y <= rmaxy + cornerSize) {
      CanSouthWestResize(this)
    }
    else if (rmaxx - cornerSize <= x && x <= rmaxx + cornerSize
      && rmaxy - cornerSize <= y && y <= rmaxy + cornerSize) {
      CanSouthEastResize(this)
    }
    else if (rminx - cornerSize <= x && x <= rminx + cornerSize) {
      CanWestResize(this)
    }
    else if (rmaxx - cornerSize <= x && x <= rmaxx + cornerSize) {
      CanEastResize(this)
    }
    else if (rminy - cornerSize <= y && y <= rminy + cornerSize) {
      CanNorthResize(this)
    }
    else if (rmaxy - cornerSize <= y && y <= rmaxy + cornerSize) {
      CanSouthResize(this)
    }
    else if (rect.contains(x, y)) {
      CanMove(this)
    }
    else CanDoNothing
  }

  override def toLeft: LeftCropField = LeftCropFieldImpl(originalSize, rect)
  override def toRight: RightCropField = RightCropFieldImpl(originalSize, rect)
  override def toBottom: BottomCropField = BottomCropFieldImpl(originalSize, rect)
  override def toTop: TopCropField = TopCropFieldImpl(originalSize, rect)
  override def asJson: JsObject = Json.obj(
    "originalSize" -> JsArray(Seq(JsNumber(originalSize._1), JsNumber(originalSize._2))),
    "x" -> rect.getMinX(),
    "y" -> rect.getMinY(),
    "w" -> rect.getWidth(),
    "h" -> rect.getHeight()
  )
}

case class UnknownCropFieldImpl(originalSize: (Double, Double), rect: Rectangle2D) extends CropFieldImpl(originalSize, rect) {
  override type R = UnknownCropFieldImpl
  def withNewRect(newRect: Rectangle2D, formSize: (Double, Double)): R = copy(rect = newRect, originalSize = formSize)
}

case class TopCropFieldImpl(originalSize: (Double, Double), rect: Rectangle2D) extends CropFieldImpl(originalSize, rect) with TopCropField {
  override type R = TopCropFieldImpl
  override def toTop: TopCropField = this
  override def draw(formSize: (Double, Double), gc: SfxGraphicsContext, isSelected: Boolean) {
    import CropFieldImpl.LineWidth

    super.draw(formSize, gc, isSelected)
    if (rect.height > LineWidth && rect.width > LineWidth) {
      val rminx = scaleX(formSize, rect.minX)
      val rmaxx = scaleX(formSize, rect.maxX)
      val w = scaleX(formSize, rect.width)
      val rminy = scaleY(formSize, rect.minY)
      val rmaxy = scaleY(formSize, rect.maxY)
      gc.strokeLine(
        rminx + LineWidth / 2, rminy + LineWidth / 2,
        rminx + w / 2 - LineWidth / 2, rmaxy - LineWidth / 2
      )
      gc.strokeLine(
        rminx + w / 2 - LineWidth / 2, rmaxy - LineWidth / 2,
        rmaxx - LineWidth / 2, rminy + LineWidth / 2
      )
    }
  }
  def withNewRect(newRect: Rectangle2D, formSize: (Double, Double)): R = copy(rect = newRect, originalSize = formSize)
}

case class LeftCropFieldImpl(
  originalSize: (Double, Double), rect: Rectangle2D
) extends CropFieldImpl(originalSize, rect) with LeftCropField {
  type R = LeftCropFieldImpl
  override def toLeft: LeftCropField = this
  override def draw(formSize: (Double, Double), gc: SfxGraphicsContext, isSelected: Boolean) {
    import CropFieldImpl.LineWidth

    super.draw(formSize, gc, isSelected)
    val h = scaleY(formSize, rect.height)
    val w = scaleX(formSize, rect.width)
    val rminx = scaleX(formSize, rect.minX)
    val rmaxx = scaleX(formSize, rect.maxX)
    val rminy = scaleY(formSize, rect.minY)
    val rmaxy = scaleY(formSize, rect.maxY)
    if (h > LineWidth && w > LineWidth) {
      gc.strokeLine(
        rminx + LineWidth / 2, rminy + LineWidth / 2,
        rmaxx - LineWidth / 2, rminy + h / 2 - LineWidth / 2
      )
      gc.strokeLine(
        rmaxx - LineWidth / 2, rminy + h / 2 - LineWidth / 2,
        rminx + LineWidth / 2, rmaxy - LineWidth / 2
      )
    }
  }
  def withNewRect(newRect: Rectangle2D, formSize: (Double, Double)): R = copy(rect = newRect, originalSize = formSize)
}

case class RightCropFieldImpl(
  originalSize: (Double, Double), rect: Rectangle2D
) extends CropFieldImpl(originalSize, rect) with RightCropField {
  type R = RightCropFieldImpl
  override def toRight: RightCropField = this
  override def draw(formSize: (Double, Double), gc: SfxGraphicsContext, isSelected: Boolean) {
    import CropFieldImpl.LineWidth

    super.draw(formSize, gc, isSelected)
    val h = scaleY(formSize, rect.height)
    val w = scaleX(formSize, rect.width)
    val rminx = scaleX(formSize, rect.minX)
    val rmaxx = scaleX(formSize, rect.maxX)
    val rminy = scaleY(formSize, rect.minY)
    val rmaxy = scaleY(formSize, rect.maxY)
    if (h > LineWidth && w > LineWidth) {
      gc.strokeLine(
        rmaxx - LineWidth / 2, rminy + LineWidth / 2,
        rminx + LineWidth / 2, rminy + h / 2 - LineWidth / 2
      )
      gc.strokeLine(
        rminx + LineWidth / 2, rminy + h / 2 - LineWidth / 2,
        rmaxx - LineWidth / 2, rmaxy - LineWidth / 2
      )
    }
  }
  def withNewRect(newRect: Rectangle2D, formSize: (Double, Double)): R = copy(rect = newRect, originalSize = formSize)
}

case class BottomCropFieldImpl(
  originalSize: (Double, Double), rect: Rectangle2D
) extends CropFieldImpl(originalSize, rect) with BottomCropField {
  type R = BottomCropFieldImpl
  override def toBottom: BottomCropField = this
  override def draw(formSize: (Double, Double), gc: SfxGraphicsContext, isSelected: Boolean) {
    import CropFieldImpl.LineWidth

    super.draw(formSize, gc, isSelected)
    val h = scaleY(formSize, rect.height)
    val w = scaleX(formSize, rect.width)
    val rminx = scaleX(formSize, rect.minX)
    val rmaxx = scaleX(formSize, rect.maxX)
    val rminy = scaleY(formSize, rect.minY)
    val rmaxy = scaleY(formSize, rect.maxY)
    if (h > LineWidth && w > LineWidth) {
      gc.strokeLine(
        rminx + LineWidth / 2, rmaxy - LineWidth / 2,
        rminx - LineWidth / 2 + w / 2, rminy + LineWidth / 2
      )
      gc.strokeLine(
        rminx - LineWidth / 2 + w / 2, rminy + LineWidth / 2,
        rmaxx - LineWidth / 2, rmaxy - LineWidth / 2
      )
    }
  }
  def withNewRect(newRect: Rectangle2D, formSize: (Double, Double)): this.type = copy(rect = newRect, originalSize = formSize).asInstanceOf[this.type]
}

case class SkewCorrectionConditionImpl(
  direction: SkewCorrectionDirection = SkewCorrectionDirectionHorizontal,
  errorAllowance: Int = 25,
  lineCount: Int = 1,
  maxAngleToDetect: Double = 2.0,
  detectAreaFrom: Percent = Percent(0),
  detectAreaTo: Percent = Percent(100)
) extends SkewCorrectionCondition

object SkewCorrectionConditionImpl {
  import com.ruimo.forms.SkewCorrection.percentFormat

  implicit val skewCorrectionConditionImplWrites = new Writes[SkewCorrectionConditionImpl] {
    def writes(obj: SkewCorrectionConditionImpl): JsObject = Json.obj(
      "direction" -> Json.toJson(obj.direction),
      "errorAllowance" -> JsNumber(obj.errorAllowance),
      "lineCount" -> JsNumber(obj.lineCount),
      "maxAngleToDetect" -> JsNumber(obj.maxAngleToDetect),
      "detectAreaFrom" -> JsNumber(obj.detectAreaFrom.value),
      "detectAreaTo" -> JsNumber(obj.detectAreaTo.value)
    )
  }

  implicit val skewCorrectionConditionImplReads: Reads[SkewCorrectionConditionImpl] = (
    (JsPath \ "direction").read[SkewCorrectionDirection] and
    (JsPath \ "errorAllowance").read[Int] and
    (JsPath \ "lineCount").read[Int] and
    (JsPath \ "maxAngleToDetect").read[Double] and
    (JsPath \ "detectAreaFrom").readWithDefault(Percent(0)) and
    (JsPath \ "detectAreaTo").readWithDefault(Percent(100))
  )(SkewCorrectionConditionImpl.apply _)
}

case class DotRemovalConditionImpl(
  maxDotSize: Int = 10,
  blackLevel: BlackLevel = BlackLevel(200)
) extends DotRemovalCondition

object DotRemovalConditionImpl {
  implicit val dotRemovalConditionImplWrites = new Writes[DotRemovalConditionImpl] {
    def writes(obj: DotRemovalConditionImpl): JsObject = Json.obj(
      "maxDotSize" -> JsNumber(obj.maxDotSize),
      "blackLevel" -> JsNumber(obj.blackLevel.value)
    )
  }

  implicit val dotRemovalConditionImplReads: Reads[DotRemovalConditionImpl] = (
    (JsPath \ "maxDotSize").read[Int] and
    (JsPath \ "blackLevel").read[BlackLevel]
  )(DotRemovalConditionImpl.apply _)
}

case class CropRectangleConditionImpl(
  errorAllowance: Int = 25,
  topMargin: Double = 0,
  leftMargin: Double = 0,
  rightMargin: Double = 0,
  bottomMargin: Double = 0,
  slantAllowance: Int = 1
) extends CropRectangleCondition

object CropRectangleConditionImpl {
  implicit val cropRectangleConditionImplWrites = new Writes[CropRectangleConditionImpl] {
    def writes(obj: CropRectangleConditionImpl): JsObject = Json.obj(
      "errorAllowance" -> JsNumber(obj.errorAllowance),
      "topMargin" -> JsNumber(obj.topMargin),
      "leftMargin" -> JsNumber(obj.leftMargin),
      "rightMargin" -> JsNumber(obj.rightMargin),
      "bottomMargin" -> JsNumber(obj.bottomMargin),
      "slantAllowance" -> JsNumber(obj.slantAllowance)
    )
  }

  implicit val cropRectangleConditionImplReads: Reads[CropRectangleConditionImpl] = (
    (JsPath \ "errorAllowance").read[Int] and
    (JsPath \ "topMargin").read[Double] and
    (JsPath \ "leftMargin").read[Double] and
    (JsPath \ "rightMargin").read[Double] and
    (JsPath \ "bottomMargin").read[Double] and
    (JsPath \ "slantAllowance").readWithDefault[Int](2)
  )(CropRectangleConditionImpl.apply _)
}

case class EdgeCropConditionImpl(
  topArea: Option[Area],
  bottomArea: Option[Area],
  leftArea: Option[Area],
  rightArea: Option[Area],
  topSensivity: EdgeCropSensivity,
  bottomSensivity: EdgeCropSensivity,
  rightSensivity: EdgeCropSensivity,
  leftSensivity: EdgeCropSensivity
) extends EdgeCropCondition {
  lazy val asJson: JsObject = {
    def areaToJson(area: Area) = {
      JsArray(
        Seq(
          JsNumber(area.x.value), JsNumber(area.y.value), JsNumber(area.w.value), JsNumber(area.h.value)
        )
      )
    }

    JsObject(
      topArea.map { a => "top" -> areaToJson(a) }.toSeq ++
      bottomArea.map { a => "bottom" -> areaToJson(a) }.toSeq ++
      leftArea.map { a => "left" -> areaToJson(a) }.toSeq ++
      rightArea.map { a => "right" -> areaToJson(a) }.toSeq
    ) ++ Json.obj(
      "topSensivity" -> JsNumber(topSensivity.value),
      "bottomSensivity" -> JsNumber(bottomSensivity.value),
      "leftSensivity" -> JsNumber(leftSensivity.value),
      "rightSensivity" -> JsNumber(rightSensivity.value)
    )
  }
}

object ProjectImpl {
  def apply(
    projectContext: ProjectContext,
    listener: ProjectListener = NullObjectListener
  ): ProjectImpl = new ProjectImpl(projectContext, listener)
}

class ProjectImpl(
  projectContext: ProjectContext,
  val listener: ProjectListener
) extends com.ruimo.forms.Project {
  val version = VersionOne
  @volatile
  private[this] var _isDirty = false
  @volatile
  private[this] var _skewCorrection: SkewCorrection = SkewCorrection(enabled = false, SkewCorrectionConditionImpl())
  @volatile
  private[this] var _dotRemoval: DotRemoval = DotRemoval(enabled = false, DotRemovalConditionImpl())
  @volatile
  private[this] var _cropRectangle: CropRectangle = CropRectangle(enabled = false, CropRectangleConditionImpl())
  @volatile
  private[this] var _absFields = new AbsoluteFieldTable(projectContext)
  @volatile
  private[this] var _leftCropField: Option[LeftCropField] = None
  @volatile
  private[this] var _rightCropField: Option[RightCropField] = None
  @volatile
  private[this] var _topCropField: Option[TopCropField] = None
  @volatile
  private[this] var _bottomCropField: Option[BottomCropField] = None
  @volatile
  private[this] var _isLeftCropFieldSelected: Boolean = false
  @volatile
  private[this] var _isRightCropFieldSelected: Boolean = false
  @volatile
  private[this] var _isTopCropFieldSelected: Boolean = false
  @volatile
  private[this] var _isBottomCropFieldSelected: Boolean = false
  @volatile
  private[this] var _isEdgeCropEnabled: Boolean = false
  @volatile
  private[this] var _cachedImage: imm.Map[(Path, CacheCondition), RetrievePreparedImageResultOk] = Map()
  @volatile
  private[this] var _topSensivity: EdgeCropSensivity = EdgeCropSensivity(254)
  @volatile
  private[this] var _bottomSensivity: EdgeCropSensivity = EdgeCropSensivity(254)
  @volatile
  private[this] var _leftSensivity: EdgeCropSensivity = EdgeCropSensivity(254)
  @volatile
  private[this] var _rightSensivity: EdgeCropSensivity = EdgeCropSensivity(254)

  def isDirty = _isDirty

  def withListener(newListener: ProjectListener): Project = new ProjectImpl(
    this.projectContext,
    newListener
  )

  def absoluteFields: AbsoluteFieldTable = _absFields

  def skewCorrection: SkewCorrection = _skewCorrection

  def skewCorrection_=(newSkewCorrection: SkewCorrection) {
    if (newSkewCorrection != _skewCorrection) {
      this._skewCorrection = newSkewCorrection
      listener.onSkewCorrectionChanged(newSkewCorrection)
      _isDirty = true
    }
  }

  def dotRemoval: DotRemoval = _dotRemoval

  def dotRemoval_=(newDotRemoval: DotRemoval): Unit = {
    if (newDotRemoval != _dotRemoval) {
      this._dotRemoval = newDotRemoval
      listener.onDotRemovalChanged(newDotRemoval)
      _isDirty = true
    }
  }
  
  def cropRectangle: CropRectangle = _cropRectangle

  def cropRectangle_=(newCropRectangle: CropRectangle): Unit = {
    if (newCropRectangle != _cropRectangle) {
      this._cropRectangle = newCropRectangle
      listener.onCropRectangleChanged(newCropRectangle)
      _isDirty = true
    }
  }
  
  def addAbsoluteField(f: AbsoluteField, isSelected: Boolean, redraw: Boolean = true) {
    _absFields.addAbsoluteField(f, isSelected, redraw)
    _isDirty = true
  }

  def deselectAllFields() {
    _absFields.deselectAllFields()
    selectBottomCropField(false)
    selectRightCropField(false)
    selectTopCropField(false)
    selectLeftCropField(false)
  }

  def selectAbsoluteFields(formSize: (Double, Double), rect: Rectangle2D, e: MouseEvent) {
    _absFields.selectFields(formSize, rect, e)
  }

  def selectCropFields(formSize: (Double, Double), rect: Rectangle2D, e: MouseEvent) {
    if (! isTopCropFieldSelected) {
      _topCropField.foreach { f =>
        if (f.intersects(formSize, rect)) {
          selectTopCropField(true)
        }
      }
    }

    if (! isLeftCropFieldSelected) {
      _leftCropField.foreach { f =>
        if (f.intersects(formSize, rect)) {
          selectLeftCropField(true)
        }
      }
    }

    if (! isRightCropFieldSelected) {
      _rightCropField.foreach { f =>
        if (f.intersects(formSize, rect)) {
          selectRightCropField(true)
        }
      }
    }

    if (! isBottomCropFieldSelected) {
      _bottomCropField.foreach { f =>
        if (f.intersects(formSize, rect)) {
          selectBottomCropField(true)
        }
      }
    }
  }

  def selectSingleFieldAt(formSize: (Double, Double), x: Double, y: Double) {
    if (_absFields.selectSingleAbsoluteFieldAt(formSize, x, y).isEmpty) {
      leftCropField.find { _.drawArea(formSize).contains(x, y) }.orElse {
        topCropField.find { _.drawArea(formSize).contains(x, y) }.orElse {
          rightCropField.find { _.drawArea(formSize).contains(x, y) }.orElse {
            bottomCropField.find { _.drawArea(formSize).contains(x, y) }
          }
        }
      }.foreach { cf =>
        projectContext.onSelectedCropFieldAdded(cf)
      }
    }
  }

  def selectField(f: Field) {
    f match {
      case af: AbsoluteField => selectAbsoluteField(af)
      case cf: CropField => selectCropField(cf)
    }
  }

  def selectCropField(f: CropField) {
    f match {
      case lf: LeftCropField => selectLeftCropField(true)
      case rf: RightCropField => selectRightCropField(true)
      case tf: TopCropField => selectTopCropField(true)
      case bf: BottomCropField => selectBottomCropField(true)
    }
  }

  def selectAbsoluteField(f: AbsoluteField) {
    _absFields.selectAbsoluteField(f)
  }

  def deleteAllSelectedFields() {
    _absFields.deleteAllSelectedFields()
    _isDirty = true
  }

  def getSelectedFieldAt(formSize: (Double, Double), x: Double, y: Double): Option[Field] =
    getSelectedAbsoluteFieldAt(formSize, x, y).orElse(getSelectedCropFieldAt(formSize, x, y))

  def getSelectedAbsoluteFieldAt(formSize: (Double, Double), x: Double, y: Double): Option[AbsoluteField] =
    _absFields.getSelectedFieldAt(formSize, x, y)

  def getCropFieldAt(formSize: (Double, Double), x: Double, y: Double, isSelected: Boolean): Option[CropField] =
    _leftCropField.filter(isSelected == isLeftCropFieldSelected && _.contains(formSize, x, y)).orElse(
      _rightCropField.filter(isSelected == isRightCropFieldSelected && _.contains(formSize, x, y))
    ).orElse(
      _topCropField.filter(isSelected == isTopCropFieldSelected && _.contains(formSize, x, y))
    ).orElse(
      _bottomCropField.filter(isSelected == isBottomCropFieldSelected && _.contains(formSize, x, y))
    )

  def getSelectedCropFieldAt(formSize: (Double, Double), x: Double, y: Double): Option[CropField] = getCropFieldAt(formSize, x, y, isSelected = true)

  def getNormalFieldAt(formSize: (Double, Double), x: Double, y: Double): Option[Field] =
    getNormalAbsoluteFieldAt(formSize, x, y).orElse(getNormalCropFieldAt(formSize, x, y))

  def getNormalAbsoluteFieldAt(formSize: (Double, Double), x: Double, y: Double): Option[AbsoluteField] =
    _absFields.getNormalFieldAt(formSize, x, y)

  def getNormalCropFieldAt(formSize: (Double, Double), x: Double, y: Double): Option[CropField] = getCropFieldAt(formSize, x, y, isSelected = false)

  def moveSelectedFields(formSize: (Double, Double), from: Point2D, to: Point2D) {
    moveSelectedAbsoluteFields(formSize, from, to)
    moveSelectedCropFields(formSize, from, to)
    _isDirty = true
  }

  def northResizeSelectedFields(formSize: (Double, Double), from: Point2D, to: Point2D) {
    northResizeSelectedAbsoluteFields(formSize, from, to)
    northResizeSelectedCropFields(formSize, from, to)
    _isDirty = true
  }

  def eastResizeSelectedFields(formSize: (Double, Double), from: Point2D, to: Point2D) {
    eastResizeSelectedAbsoluteFields(formSize, from, to)
    eastResizeSelectedCropFields(formSize, from, to)
    _isDirty = true
  }

  def westResizeSelectedFields(formSize: (Double, Double), from: Point2D, to: Point2D) {
    westResizeSelectedAbsoluteFields(formSize, from, to)
    westResizeSelectedCropFields(formSize, from, to)
    _isDirty = true
  }

  def southResizeSelectedFields(formSize: (Double, Double), from: Point2D, to: Point2D) {
    southResizeSelectedAbsoluteFields(formSize, from, to)
    southResizeSelectedCropFields(formSize, from, to)
    _isDirty = true
  }

  def northWestResizeSelectedFields(formSize: (Double, Double), from: Point2D, to: Point2D) {
    northWestResizeSelectedAbsoluteFields(formSize, from, to)
    northWestResizeSelectedCropFields(formSize, from, to)
    _isDirty = true
  }

  def northEastResizeSelectedFields(formSize: (Double, Double), from: Point2D, to: Point2D) {
    northEastResizeSelectedAbsoluteFields(formSize, from, to)
    northEastResizeSelectedCropFields(formSize, from, to)
    _isDirty = true
  }

  def southWestResizeSelectedFields(formSize: (Double, Double), from: Point2D, to: Point2D) {
    southWestResizeSelectedAbsoluteFields(formSize, from, to)
    southWestResizeSelectedCropFields(formSize, from, to)
    _isDirty = true
  }

  def southEastResizeSelectedFields(formSize: (Double, Double), from: Point2D, to: Point2D) {
    southEastResizeSelectedAbsoluteFields(formSize, from, to)
    southEastResizeSelectedCropFields(formSize, from, to)
    _isDirty = true
  }

  def moveSelectedAbsoluteFields(formSize: (Double, Double), from: Point2D, to: Point2D) {
    _absFields.moveSelectedFields(formSize, from, to)
    _isDirty = true
  }

  def modifySelectedCropField[T <: CropField](
    field: Option[T], isSelected: Boolean, modifier: T => T
  ) {
    println("modifySelectedCropField()")
    var fieldModified = false
    field.foreach { f =>
      if (isSelected) {
        fieldModified = true
        val modified = modifier(f)
        addCropField(modified, selected = true)
        projectContext.onSelectedCropFieldRemoved(f)
        projectContext.onSelectedCropFieldAdded(modified)
      }
    }

    if (fieldModified) {
      invalidateCachedImage(CacheConditionGlob(isCropEnabled = Some(true)))
      _isDirty = true
    }
  }

  def moveSelectedCropFields(formSize: (Double, Double), from: Point2D, to: Point2D) {
    println("moveSelectedCropFields()")
    val modifier: CropField => CropField = _.move(formSize, from, to)
    modifySelectedCropField(topCropField, isTopCropFieldSelected, modifier)
    modifySelectedCropField(leftCropField, isLeftCropFieldSelected, modifier)
    modifySelectedCropField(rightCropField, isRightCropFieldSelected, modifier)
    modifySelectedCropField(bottomCropField, isBottomCropFieldSelected, modifier)
  }

  def northResizeSelectedAbsoluteFields(formSize: (Double, Double), from: Point2D, to: Point2D) {
    _absFields.northResizeSelectedFields(formSize, from, to)
    _isDirty = true
  }

  def eastResizeSelectedAbsoluteFields(formSize: (Double, Double), from: Point2D, to: Point2D) {
    _absFields.eastResizeSelectedFields(formSize, from, to)
    _isDirty = true
  }

  def westResizeSelectedAbsoluteFields(formSize: (Double, Double), from: Point2D, to: Point2D) {
    _absFields.westResizeSelectedFields(formSize, from, to)
    _isDirty = true
  }

  def southResizeSelectedAbsoluteFields(formSize: (Double, Double), from: Point2D, to: Point2D) {
    _absFields.southResizeSelectedFields(formSize, from, to)
    _isDirty = true
  }

  def northEastResizeSelectedAbsoluteFields(formSize: (Double, Double), from: Point2D, to: Point2D) {
    _absFields.northEastResizeSelectedFields(formSize, from, to)
    _isDirty = true
  }

  def northWestResizeSelectedAbsoluteFields(formSize: (Double, Double), from: Point2D, to: Point2D) {
    _absFields.northWestResizeSelectedFields(formSize, from, to)
    _isDirty = true
  }

  def southEastResizeSelectedAbsoluteFields(formSize: (Double, Double), from: Point2D, to: Point2D) {
    _absFields.southEastResizeSelectedFields(formSize, from, to)
    _isDirty = true
  }

  def southWestResizeSelectedAbsoluteFields(formSize: (Double, Double), from: Point2D, to: Point2D) {
    _absFields.southWestResizeSelectedFields(formSize, from, to)
    _isDirty = true
  }

  def northResizeSelectedCropFields(formSize: (Double, Double), from: Point2D, to: Point2D) {
    println("northResizeSelectedCropFields()")
    val modifier: CropField => CropField = _.northResize(formSize, from, to)
    modifySelectedCropField(topCropField, isTopCropFieldSelected, modifier)
    modifySelectedCropField(leftCropField, isLeftCropFieldSelected, modifier)
    modifySelectedCropField(rightCropField, isRightCropFieldSelected, modifier)
    modifySelectedCropField(bottomCropField, isBottomCropFieldSelected, modifier)
  }

  def eastResizeSelectedCropFields(formSize: (Double, Double), from: Point2D, to: Point2D) {
    println("eastResizeSelectedCropFields()")
    val modifier: CropField => CropField = _.eastResize(formSize, from, to)
    modifySelectedCropField(topCropField, isTopCropFieldSelected, modifier)
    modifySelectedCropField(leftCropField, isLeftCropFieldSelected, modifier)
    modifySelectedCropField(rightCropField, isRightCropFieldSelected, modifier)
    modifySelectedCropField(bottomCropField, isBottomCropFieldSelected, modifier)
  }

  def westResizeSelectedCropFields(formSize: (Double, Double), from: Point2D, to: Point2D) {
    println("westResizeSelectedCropFields()")
    val modifier: CropField => CropField = _.westResize(formSize, from, to)
    modifySelectedCropField(topCropField, isTopCropFieldSelected, modifier)
    modifySelectedCropField(leftCropField, isLeftCropFieldSelected, modifier)
    modifySelectedCropField(rightCropField, isRightCropFieldSelected, modifier)
    modifySelectedCropField(bottomCropField, isBottomCropFieldSelected, modifier)
  }

  def southResizeSelectedCropFields(formSize: (Double, Double), from: Point2D, to: Point2D) {
    println("southResizeSelectedCropFields()")
    val modifier: CropField => CropField = _.southResize(formSize, from, to)
    modifySelectedCropField(topCropField, isTopCropFieldSelected, modifier)
    modifySelectedCropField(leftCropField, isLeftCropFieldSelected, modifier)
    modifySelectedCropField(rightCropField, isRightCropFieldSelected, modifier)
    modifySelectedCropField(bottomCropField, isBottomCropFieldSelected, modifier)
  }

  def northEastResizeSelectedCropFields(formSize: (Double, Double), from: Point2D, to: Point2D) {
    println("northEastResizeSelectedCropFields()")
    val modifier: CropField => CropField = _.northEastResize(formSize, from, to)
    modifySelectedCropField(topCropField, isTopCropFieldSelected, modifier)
    modifySelectedCropField(leftCropField, isLeftCropFieldSelected, modifier)
    modifySelectedCropField(rightCropField, isRightCropFieldSelected, modifier)
    modifySelectedCropField(bottomCropField, isBottomCropFieldSelected, modifier)
  }

  def northWestResizeSelectedCropFields(formSize: (Double, Double), from: Point2D, to: Point2D) {
    println("northWestResizeSelectedCropFields()")
    val modifier: CropField => CropField = _.northWestResize(formSize, from, to)
    modifySelectedCropField(topCropField, isTopCropFieldSelected, modifier)
    modifySelectedCropField(leftCropField, isLeftCropFieldSelected, modifier)
    modifySelectedCropField(rightCropField, isRightCropFieldSelected, modifier)
    modifySelectedCropField(bottomCropField, isBottomCropFieldSelected, modifier)
  }

  def southEastResizeSelectedCropFields(formSize: (Double, Double), from: Point2D, to: Point2D) {
    println("southEastResizeSelectedCropFields()")
    val modifier: CropField => CropField = _.southEastResize(formSize, from, to)
    modifySelectedCropField(topCropField, isTopCropFieldSelected, modifier)
    modifySelectedCropField(leftCropField, isLeftCropFieldSelected, modifier)
    modifySelectedCropField(rightCropField, isRightCropFieldSelected, modifier)
    modifySelectedCropField(bottomCropField, isBottomCropFieldSelected, modifier)
  }

  def southWestResizeSelectedCropFields(formSize: (Double, Double), from: Point2D, to: Point2D) {
    println("southWestResizeSelectedCropFields()")
    val modifier: CropField => CropField = _.southWestResize(formSize, from, to)
    modifySelectedCropField(topCropField, isTopCropFieldSelected, modifier)
    modifySelectedCropField(leftCropField, isLeftCropFieldSelected, modifier)
    modifySelectedCropField(rightCropField, isRightCropFieldSelected, modifier)
    modifySelectedCropField(bottomCropField, isBottomCropFieldSelected, modifier)
  }

  def renameSelectedAbsoluteField(f: AbsoluteField, newName: String) {
    _absFields.renameSelectedField(f, newName)
    _isDirty = true
  }

  def onCropFieldRemoved(f: CropField, isSelected: Boolean) {
    if (isSelected) {
      projectContext.onSelectedCropFieldRemoved(f)
    }
    else {
      projectContext.onNormalCropFieldRemoved(f)
    }
    _isDirty = true
  }

  def onCropFieldAdded(f: CropField, isSelected: Boolean) {
    if (isSelected) {
      projectContext.onSelectedCropFieldAdded(f)
    }
    else {
      projectContext.onNormalCropFieldAdded(f)
    }
    _isDirty = true
  }

  def redrawCropField(f: CropField, isSelected: Boolean) {
    projectContext.redrawCropField(f)
  }

  def redraw() {
    _absFields.redraw()
    if (! cropEnabled) {
      topCropField.foreach { f =>
        redrawCropField(f, isTopCropFieldSelected)
      }
      leftCropField.foreach { f =>
        redrawCropField(f, isLeftCropFieldSelected)
      }
      rightCropField.foreach { f =>
        redrawCropField(f, isRightCropFieldSelected)
      }
      bottomCropField.foreach { f =>
        redrawCropField(f, isBottomCropFieldSelected)
      }
    }
  }

  def possibleMouseOperation(formSize: (Double, Double), x: Double, y: Double): MouseOperation = {
    var ret = _absFields.possibleMouseOperation(formSize, x, y)
    if (ret != CanDoNothing) return ret

    ret = topCropField.map { _.possibleMouseOperation(formSize, x, y) }.getOrElse(CanDoNothing)
    if (ret != CanDoNothing) return ret

    ret = leftCropField.map { _.possibleMouseOperation(formSize, x, y) }.getOrElse(CanDoNothing)
    if (ret != CanDoNothing) return ret

    ret = rightCropField.map { _.possibleMouseOperation(formSize, x, y) }.getOrElse(CanDoNothing)
    if (ret != CanDoNothing) return ret

    ret = bottomCropField.map { _.possibleMouseOperation(formSize, x, y) }.getOrElse(CanDoNothing)
    if (ret != CanDoNothing) return ret

    CanDoNothing
  }

  // returns old field
  def addLeftCropField(f: LeftCropField, selected: Boolean, redraw: Boolean = true): Option[LeftCropField] = {
    val existing = this._leftCropField
    this._leftCropField = Some(f)
    this._isLeftCropFieldSelected = selected
    if (redraw) {
      existing.foreach { f => onCropFieldRemoved(f, isLeftCropFieldSelected) }
      onCropFieldAdded(f, selected)
    }
    existing
  }

  // returns old field
  def addRightCropField(f: RightCropField, selected: Boolean, redraw: Boolean = true): Option[RightCropField] = {
    val existing = this._rightCropField
    this._rightCropField = Some(f)
    this._isRightCropFieldSelected = selected
    if (redraw) {
      existing.foreach { f => onCropFieldRemoved(f, isRightCropFieldSelected) }
      onCropFieldAdded(f, selected)
    }
    existing
  }

  // returns old field
  def addTopCropField(f: TopCropField, selected: Boolean, redraw: Boolean = true): Option[TopCropField] = {
    val existing = this._topCropField
    this._topCropField = Some(f)
    this._isTopCropFieldSelected = selected
    if (redraw) {
      existing.foreach { f => onCropFieldRemoved(f, isTopCropFieldSelected) }
      onCropFieldAdded(f, selected)
    }
    existing
  }

  // returns old field
  def addBottomCropField(f: BottomCropField, selected: Boolean, redraw: Boolean): Option[BottomCropField] = {
    val existing = this._bottomCropField
    this._bottomCropField = Some(f)
    this._isBottomCropFieldSelected = selected
    if (redraw) {
      existing.foreach { f => onCropFieldRemoved(f, isBottomCropFieldSelected) }
      onCropFieldAdded(f, selected)
    }
    existing
  }

  def addCropField[T <: CropField](f: T, selected: Boolean): Option[CropField] = f match {
    case top: TopCropField => addTopCropField(top, selected)
    case left: LeftCropField => addLeftCropField(left, selected)
    case right: RightCropField => addRightCropField(right, selected)
    case bottom: BottomCropField => addBottomCropField(bottom, selected)
  }

  def leftCropField: Option[LeftCropField] = _leftCropField
  def topCropField: Option[TopCropField] = _topCropField
  def rightCropField: Option[RightCropField] = _rightCropField
  def bottomCropField: Option[BottomCropField] = _bottomCropField

  def isTopCropFieldSelected: Boolean = _isTopCropFieldSelected
  def isLeftCropFieldSelected: Boolean = _isLeftCropFieldSelected
  def isRightCropFieldSelected: Boolean = _isRightCropFieldSelected
  def isBottomCropFieldSelected: Boolean = _isBottomCropFieldSelected

  def selectLeftCropField(selected: Boolean) {
    leftCropField.foreach { cf =>
      _isLeftCropFieldSelected = selected
      projectContext.selectCropField(cf, selected)
    }
  }

  def selectTopCropField(selected: Boolean) {
    topCropField.foreach { cf =>
      _isTopCropFieldSelected = selected
      projectContext.selectCropField(cf, selected)
    }
  }

  def selectRightCropField(selected: Boolean) {
    rightCropField.foreach { cf =>
      _isRightCropFieldSelected = selected
      projectContext.selectCropField(cf, selected)
    }
  }

  def selectBottomCropField(selected: Boolean) {
    bottomCropField.foreach { cf =>
      _isBottomCropFieldSelected = selected
      projectContext.selectCropField(cf, selected)
    }
  }

  def cropEnabled_=(enabled: Boolean) {
    _isEdgeCropEnabled = enabled
    if (enabled) {
      _isBottomCropFieldSelected = false
      _isTopCropFieldSelected = false
      _isLeftCropFieldSelected = false
      _isRightCropFieldSelected = false
    }
    listener.onCropEnabledChanged(enabled)
    _isDirty = true
  }

  def cropEnabled: Boolean = _isEdgeCropEnabled

  def edgeCrop(
    formWidth: Double, formHeight: Double,
    topSensivity: EdgeCropSensivity, bottomSensivity: EdgeCropSensivity, leftSensivity: EdgeCropSensivity, rightSensivity: EdgeCropSensivity
  ): EdgeCrop = {
    def cropFieldToArea(f: CropField): Area = {
      val rect = f.rect
      Area(
        Percent(100 * rect.minX / formWidth), Percent(100 * rect.minY / formHeight),
        Percent(100 * rect.width / formWidth), Percent(100 * rect.height / formHeight)
      )
    }

    EdgeCrop(
      _isEdgeCropEnabled,
      EdgeCropConditionImpl(
        _topCropField.map(cropFieldToArea),
        _bottomCropField.map(cropFieldToArea),
        _leftCropField.map(cropFieldToArea),
        _rightCropField.map(cropFieldToArea),
        topSensivity, bottomSensivity, rightSensivity, leftSensivity,
      )
    )
  }

  def topEdgeCropSensivity_=(topSensivity: EdgeCropSensivity) {
    println("topEdgeCropSensivity changed")
    _topSensivity = topSensivity
    _isDirty = true
    invalidateCachedImage(CacheConditionGlob(isCropEnabled = Some(true)))
  }

  def topEdgeCropSensivity: EdgeCropSensivity = _topSensivity

  def bottomEdgeCropSensivity_=(bottomSensivity: EdgeCropSensivity) {
    println("bottomEdgeCropSensivity changed")
    _bottomSensivity = bottomSensivity
    _isDirty = true
    invalidateCachedImage(CacheConditionGlob(isCropEnabled = Some(true)))
  }

  def bottomEdgeCropSensivity: EdgeCropSensivity = _bottomSensivity

  def leftEdgeCropSensivity_=(leftSensivity: EdgeCropSensivity) {
    println("leftEdgeCropSensivity changed")
    _leftSensivity = leftSensivity
    _isDirty = true
    invalidateCachedImage(CacheConditionGlob(isCropEnabled = Some(true)))
  }

  def leftEdgeCropSensivity: EdgeCropSensivity = _leftSensivity

  def rightEdgeCropSensivity_=(rightSensivity: EdgeCropSensivity) {
    println("rightEdgeCropSensivity changed")
    _rightSensivity = rightSensivity
    _isDirty = true
    invalidateCachedImage(CacheConditionGlob(isCropEnabled = Some(true)))
  }

  def rightEdgeCropSensivity: EdgeCropSensivity = _rightSensivity

  override def cachedImage(
    selectedImage: SelectedImage,
    cacheCondition: CacheCondition = CacheCondition(
      isSkewCorrectionEnabled = skewCorrection.enabled,
      isCropEnabled = cropEnabled,
      isDotRemovalEnabled = dotRemoval.enabled,
      isCropRectangleEnabled = cropRectangle.enabled
    )
  ): Either[RetrievePreparedImageRestResult, Future[RetrievePreparedImageRestResult]] = {
    println("cachedImage cacheCondition = " + cacheCondition)

    val key = (selectedImage.file, cacheCondition)
    _cachedImage.get(key) match {
      case Some(img) =>
        println("cachedImage found.")
        Left(img)
      case None =>
        println("cachedImage not found. Call server to obtain image.")
        val fimg: Future[RetrievePreparedImageRestResult] = retrievePreparedImage(selectedImage, cacheCondition)
        Right(
          fimg.map {
            case ok: RetrievePreparedImageResultOk =>
              _cachedImage = _cachedImage.updated(key, ok)
              ok
            case fail: RetrievePreparedImageRestFailure =>
              fail
          }
        )
    }
  }

  private def extention(path: Path): Option[String] = {
    val fname = path.getFileName.toString
    val idx = fname.lastIndexOf(".")
    if (idx == -1) None else Some(fname.substring(idx))
  }

  private def prepareFileForCaptureApi(image: SelectedImage): Future[Either[RetrievePreparedImageRestFailure, Path]] = {
    println("prepareFileForCaptureApi()")

    val configFile = Files.createTempFile(null, null)
    val fileName = "image001" + extention(image.file).getOrElse("")
    val cropTarget: Future[Either[RetrievePreparedImageRestFailure, (Double, Double)]] =
      if (skewCorrection.enabled) {
        val fimg: Future[RetrievePreparedImageRestResult] = cachedImage(
          image,
          CacheCondition(
            isSkewCorrectionEnabled = true,
            isCropEnabled = false,
            isDotRemovalEnabled = false,
            isCropRectangleEnabled = false
          )
        ).fold(Future.successful, identity)
        fimg.map {
          case img: RetrievePreparedImageResultOk =>
            Right(img.image.getWidth, img.image.getHeight)
          case fail: RetrievePreparedImageRestFailure =>
            Left(fail)
        }
      }
      else {
        Future.successful(Right(image.image.getWidth -> image.image.getHeight))
      }
    val captureTarget: Future[Either[RetrievePreparedImageRestFailure, (Double, Double)]] = {
      val fimg: Future[RetrievePreparedImageRestResult] =
        cachedImage(
          image, CacheCondition(
            isSkewCorrectionEnabled = skewCorrection.enabled,
            isCropEnabled = cropEnabled,
            isDotRemovalEnabled = dotRemoval.enabled,
            isCropRectangleEnabled = cropRectangle.enabled
          )
        ).fold(Future.successful, identity)
      fimg.map {
        case img: RetrievePreparedImageResultOk =>
          Right(img.image.getWidth, img.image.getHeight)
        case fail: RetrievePreparedImageRestFailure =>
          Left(fail)
      }
    }

    val json: Future[Either[RetrievePreparedImageRestFailure, JsObject]] =
      cropTarget.flatMap { (crpt: Either[RetrievePreparedImageRestFailure, (Double, Double)]) =>
        captureTarget.map { (cp: Either[RetrievePreparedImageRestFailure, (Double, Double)]) =>
          crpt.flatMap { (cropt: (Double, Double)) =>
            cp.flatMap { (capt: (Double, Double)) =>
              Right(
                Json.obj(
                  "inputFiles" -> JsArray(Seq(JsString(fileName))),
                  "skewCorrection" -> Json.toJson(skewCorrection),
                  "crop" -> edgeCrop(cropt._1, cropt._2, _topSensivity, _bottomSensivity, _leftSensivity, _rightSensivity).asJson(cropEnabled),
                  "dotRemoval" -> Json.toJson(dotRemoval),
                  "cropRectangle" -> Json.toJson(cropRectangle),
                  "absoluteFields" -> _absFields.asJson
                )
              )
            }
          }
        }
      }

    json.map { (ejs: Either[RetrievePreparedImageRestFailure, JsObject]) =>
      ejs.map { js =>
        Files.write(configFile, js.toString.getBytes("utf-8"))

        val zipFile = Files.createTempFile(null, null)
        Zip.deflate(
          zipFile,
          Seq(
            "config.json" -> configFile,
            fileName -> image.file
          )
        )

        Files.delete(configFile)

        zipFile
      }
    }
  }

  private def prepareFileForPrepareApi(
    image: SelectedImage, cond: CacheCondition
  ): Future[Either[RetrievePreparedImageRestFailure, Path]] = {
    println("prepareFileForPrepareApi(cond: " + cond + ")")

    val configFile = Files.createTempFile(null, null)
    val fileName = "image001" + extention(image.file).getOrElse("")
    val cropTarget: Future[Either[RetrievePreparedImageRestFailure, (Double, Double)]] = if (! cond.isCropEnabled) {
      Future.successful(Right(0d -> 0d))
    }
    else if (skewCorrection.enabled) {
      cachedImage(
        image,
        CacheCondition(isSkewCorrectionEnabled = true, isCropEnabled = false, isDotRemovalEnabled = false, isCropRectangleEnabled = false)
      ).fold(Future.successful, identity).map {
        case ok: RetrievePreparedImageResultOk =>
          Right(ok.image.getWidth -> ok.image.getHeight)
        case fail: RetrievePreparedImageRestFailure => Left(fail)
      }
    }
    else {
      Future.successful(Right(image.image.getWidth -> image.image.getHeight))
    }

    cropTarget.map { (cpt: Either[RetrievePreparedImageRestFailure, (Double, Double)]) =>
      cpt.map { cropt =>
        val json = Json.obj(
          "inputFiles" -> JsArray(Seq(JsString(fileName))),
          "skewCorrection" -> Json.toJson(skewCorrection.copy(enabled = cond.isSkewCorrectionEnabled)),
          "crop" -> edgeCrop(cropt._1, cropt._2, _topSensivity, _bottomSensivity, _leftSensivity, _rightSensivity).asJson(cond.isCropEnabled),
          "dotRemoval" -> Json.toJson(dotRemoval.copy(enabled = cond.isDotRemovalEnabled)),
          "cropRectangle" -> Json.toJson(cropRectangle.copy(enabled = cond.isCropRectangleEnabled)),
          "ruledLineRemoval" -> Json.obj(
            "enabled" -> false
          )
        )
        println("Json to send: " + json)
        Files.write(configFile, json.toString.getBytes("utf-8"))

        val zipFile = Files.createTempFile(null, null)
        Zip.deflate(
          zipFile,
          Seq(
            "config.json" -> configFile,
            fileName -> image.file
          )
        )

        Files.delete(configFile)

        zipFile
      }
    }
  }

  def runCapture(si: SelectedImage): Future[Either[CaptureRestFailure, CaptureResultOk]] = {
    prepareFileForCaptureApi(si).map {
      case Right(fileToSubmit) =>
        val urlPath = Settings.Loader.settings.auth.url.resolve("capture")
        val auth = Settings.Loader.settings.auth

        val postResp = Await.result(
          Ws().url(urlPath).withRequestTimeout(5.minutes).addHttpHeaders(
            "Content-Type" -> "application/zip",
            "Authorization" -> (auth.contractedUserId.value + "_" + auth.applicationToken.value)
          ).post(
            Files.readAllBytes(fileToSubmit)
          ).map { resp =>
            println("status = " + resp.status)
            println("statusText = " + resp.statusText)
            resp.status match {
              case 200 =>
                val respJson = Json.parse(resp.bodyAsBytes.toArray)
                CaptureResultRunning((respJson \ "token").as[String].toLong)
              case 403 =>
                RestAuthFailure(resp.status, resp.statusText, resp.body)
              case _ =>
                RestUnknownFailure(resp.status, resp.statusText, resp.body)
            }
          },
          Duration.apply(5, TimeUnit.MINUTES)
        )

        postResp match {
          case CaptureResultRunning(token) =>
            val jobResp = getCaptureJobStatus(token, auth.contractedUserId.value.toLong, auth.applicationToken.value.toLong)
            jobResp.status match {
              case 200 =>
                val jsonResult: JsValue = Json.parse(jobResp.body)
                println("result: " + jsonResult)
                Right(CaptureResultOk.parse(jsonResult))
              case 403 =>
                Left(RestAuthFailure(jobResp.status, jobResp.statusText, jobResp.body))
              case _ =>
                Left(RestUnknownFailure(jobResp.status, jobResp.statusText, jobResp.body))
            }
        }

      case Left(fail) =>
        fail match {
          case f: RestAuthFailure => Left(f)
          case f: RestUnknownFailure => Left(f)
        }
    }
  }


  def parseResponse(f: Path): RetrievePreparedImageRestResult = {
    using(new ZipInputStream(new FileInputStream(f.toFile))) { zipIn =>
      @tailrec def parseZip(
        resp: Option[PrepareResult], finalImageFileName: Option[String], finalImage: Option[Image]
      ): RetrievePreparedImageRestResult = {
        val entry = zipIn.getNextEntry
        if (entry == null) new RetrievePreparedImageResultOk(resp, finalImage.get)
        else {
          entry.getName match {
            case "response.json" =>
              // Json.parse() closes input stream...
              val json: JsValue = using(Files.createTempFile(null, null)) { tmp =>
                Files.copy(zipIn, tmp, java.nio.file.StandardCopyOption.REPLACE_EXISTING)
                zipIn.closeEntry()
                println("Response json: " + Files.readAllLines(tmp))
                Json.parse(new FileInputStream(tmp.toFile))
              }(f => Files.delete(f)).get
              val prepareResult = PrepareResult.parse(json)
              println("prepare result: " + json)
              val fileName: Option[String] =
                prepareResult.dotRemovalResult.map {
                  case DotRemovalResultSuccess(files) => files.head
                }.orElse {
                  prepareResult.cropRectangleResult.map {
                    case CropRectangleResultSuccess(files) => files.head
                  }
                }.orElse {
                  prepareResult.cropResult.flatMap { cr =>
                    cr match {
                      case CropResultSuccess(correctedFiles) => Some(correctedFiles.head)
                      case CropResultCannotFindEdge => None
                    }
                  }
                }.orElse {
                  prepareResult.skewCorrectionResult.map { scr =>
                    scr.correctedFiles.head
                  }
                }
              println("fileName: " + fileName)
              parseZip(Some(prepareResult), fileName, finalImage)
            case fname: String =>
              if (fname == finalImageFileName.get) {
                val i = Some(new Image(zipIn))
                zipIn.closeEntry()
                parseZip(resp, finalImageFileName, i)
              }
              else {
                parseZip(resp, finalImageFileName, finalImage)
              }
          }
        }
      }

      parseZip(None, None, None)
    }.get
  }

  @tailrec private def getPrepareJobStatus(token: Long, userId: Long, appToken: Long): StandaloneWSRequest#Response = {
    val resp: StandaloneWSRequest#Response = Await.result(
      Ws().url(
        Settings.Loader.settings.auth.url.resolve("getPrepareJobStatus")
      ).addQueryStringParameters(
        "token" -> token.toString
      ).withHttpHeaders(
        "Authorization" -> ("" + userId+ "_" + appToken)
      ).get(),
      30.seconds
    )
    if (resp.status == 202) {
      Thread.sleep(1000)
      getPrepareJobStatus(token, userId, appToken)
    }
    else resp
  }

  @tailrec private def getCaptureJobStatus(token: Long, userId: Long, appToken: Long): StandaloneWSRequest#Response = {
    val resp: StandaloneWSRequest#Response = Await.result(
      Ws().url(
        Settings.Loader.settings.auth.url.resolve("getCaptureJobStatus")
      ).addQueryStringParameters(
        "token" -> token.toString
      ).withHttpHeaders(
        "Authorization" -> ("" + userId+ "_" + appToken)
      ).get(),
      30.seconds
    )
    if (resp.status == 202) {
      Thread.sleep(1000)
      getCaptureJobStatus(token, userId, appToken)
    }
    else resp
  }

  def retrievePreparedImage(
    si: SelectedImage,
    cond: CacheCondition
  ): Future[RetrievePreparedImageRestResult] = {
    println("retrievePreparedImage(cond = " + cond + ") called")
    if (
      ! cond.isSkewCorrectionEnabled && ! cond.isCropEnabled && ! cond.isDotRemovalEnabled && ! cond.isCropRectangleEnabled
    ) Future.successful(new RetrievePreparedImageResultOk(None, si.image))
    else {
      val auth = Settings.Loader.settings.auth
      prepareFileForPrepareApi(si, cond).map { (fileToSubmit: Either[RetrievePreparedImageRestFailure, Path]) =>
        fileToSubmit.map { f =>
          val urlPath = Settings.Loader.settings.auth.url.resolve("prepare")

          val postResp = Await.result(
            Ws().url(urlPath).withRequestTimeout(5.minutes).addHttpHeaders(
              "Content-Type" -> "application/zip",
              "Authorization" -> (auth.contractedUserId.value + "_" + auth.applicationToken.value)
            ).post(
              Files.readAllBytes(f)
            ).map { resp =>
              println("status = " + resp.status)
              println("statusText = " + resp.statusText)
              resp.status match {
                case 200 =>
                  val respJson = Json.parse(resp.bodyAsBytes.toArray)
                  RetrievePreparedImageResultRunning((respJson \ "token").as[String].toLong)
                case 403 =>
                  RestAuthFailure(resp.status, resp.statusText, resp.body)
                case _ =>
                  RestUnknownFailure(resp.status, resp.statusText, resp.body)
              }
            },
            Duration.apply(5, TimeUnit.MINUTES)
          )

          postResp match {
            case RetrievePreparedImageResultRunning(token) =>
              val jobResp = getPrepareJobStatus(token, auth.contractedUserId.value.toLong, auth.applicationToken.value.toLong)
              jobResp.status match {
                case 200 =>
                  PathUtil.withTempFile(None, None) { f =>
                    using(FileChannel.open(f, StandardOpenOption.CREATE, StandardOpenOption.WRITE)) { ch =>
                      ch.write(jobResp.bodyAsBytes.toByteBuffer)
                    }.get
                    parseResponse(f)
                  }.get
                case 403 =>
                  RestAuthFailure(jobResp.status, jobResp.statusText, jobResp.body)
                case _ =>
                  RestUnknownFailure(jobResp.status, jobResp.statusText, jobResp.body)
              }

            case other => other
          }
        }.fold(identity, identity)
      }
    }
  }

  def flushCachedImage() {
    println("flushCachedImage() called.")
    _cachedImage = Map()
  }

  override def invalidateCachedImage(
    cond: CacheConditionGlob = CacheConditionGlob(
      isSkewCorrectionEnabled = Some(false),
      isCropEnabled = Some(false),
      isDotRemovalEnabled = Some(false),
      isCropRectangleEnabled = Some(false)
    )
  ) {
    println("invalidateCachedImage(" + cond + ") called")
    _cachedImage = _cachedImage.filter { case (key, value) =>
      val shouldDrop = cond.isMatched(key._2)
      ! shouldDrop
    }
  }

  override def cropFieldsAreReady: Boolean =
    _topCropField.isDefined && _leftCropField.isDefined && _rightCropField.isDefined && _bottomCropField.isDefined

  private def asJson: JsValue = {
    JsObject(
      Seq(
        "skewCorrection" -> Json.toJson(skewCorrection),
        "isCropEnabled" -> JsBoolean(cropEnabled)
      ) ++ leftCropField.map(
        f => Seq("leftCropField" -> f.asJson)
      ).getOrElse(
        Seq()
      ) ++ topCropField.map(
        f => Seq("topCropField" -> f.asJson)
      ).getOrElse(
        Seq()
      ) ++ rightCropField.map(
        f => Seq("rightCropField" -> f.asJson)
      ).getOrElse(
        Seq()
      ) ++ bottomCropField.map(
        f => Seq("bottomCropField" -> f.asJson)
      ).getOrElse(
        Seq()
      )
    ) ++ Json.obj(
      "cropRectangle" -> cropRectangle
    ) ++ Json.obj(
      "dotRemoval" -> dotRemoval
    ) ++ Json.obj(
      "absoluteFields" -> absoluteFields.asJson
    )
  }

  private def prepareFileForSaveConfig(configName: String): Path = {
    println("prepareFileForSaveConfig()")

    PathUtil.withTempFile(None, None) { projectFile =>
      Files.write(projectFile, asJson.toString.getBytes("utf-8"))

      PathUtil.withTempFile(None, None) { configFile =>
        Files.write(
          configFile,
          Json.obj(
            "configName" -> configName
          ).toString.getBytes("utf-8")
        )

        val zipFile = Files.createTempFile(null, null)
        Zip.deflate(
          zipFile,
          Seq(
            "config.json" -> configFile,
            "project.json" -> projectFile
          )
        )
        zipFile
      }.get
    }.get
  }

  def saveConfig(configName: String): Future[SaveConfigRestResult] = {
    val urlPath = Settings.Loader.settings.auth.url.resolve("saveConfig")
    val auth = Settings.Loader.settings.auth

    Ws().url(urlPath).withRequestTimeout(5.minutes).addHttpHeaders(
      "Content-Type" -> "application/zip",
      "Authorization" -> (auth.contractedUserId.value + "_" + auth.applicationToken.value)
    ).post(
      Files.readAllBytes(prepareFileForSaveConfig(configName))
    ).map { resp =>
      println("status = " + resp.status)
      println("statusText = " + resp.statusText)
      resp.status match {
        case 200 =>
          val result: JsValue = Json.parse(resp.bodyAsBytes.toArray)
          SaveConfigResultOk(
            SaveConfigResult(
              Revision((result \ "revision").as[String].toLong)
            )
          )
        case 403 =>
          RestAuthFailure(resp.status, resp.statusText, resp.body)
        case _ =>
          RestUnknownFailure(resp.status, resp.statusText, resp.body)
      }
    }
  }

  private def prepareFileForListConfig(): Path = {
    println("prepareFileForListConfig()")

    PathUtil.withTempFile(None, None) { configFile =>
      Files.write(
        configFile,
        Json.obj(
          "page" -> 0,
          "pageSize" -> 1000,
          "orderBy" -> "fc0.created_at"
        ).toString.getBytes("utf-8")
      )

      val zipFile = Files.createTempFile(null, null)
      Zip.deflate(
        zipFile,
        Seq(
          "config.json" -> configFile
        )
      )
      zipFile
    }.get
  }

  def listConfig(): Future[ListConfigRestResult] = {
    val urlPath = Settings.Loader.settings.auth.url.resolve("listConfig")
    val auth = Settings.Loader.settings.auth

    Ws().url(urlPath).withRequestTimeout(5.minutes).addHttpHeaders(
      "Content-Type" -> "application/zip",
      "Authorization" -> (auth.contractedUserId.value + "_" + auth.applicationToken.value)
    ).post(
      Files.readAllBytes(prepareFileForListConfig())
    ).map { resp =>
      println("status = " + resp.status)
      println("statusText = " + resp.statusText)
      resp.status match {
        case 200 =>
          val result: JsValue = Json.parse(resp.bodyAsBytes.toArray)
          ListConfigResultOk(
            ListConfigResult(
              (result \ "records").as[Seq[JsObject]].map { e =>
                FormConfig(
                  (e \ "configName").as[String],
                  Revision((e \ "revision").as[String].toLong),
                  (e \ "comment").as[String],
                  Instant.ofEpochMilli((e \ "createdAt").as[String].toLong)
                )
              }.toList
            )
          )
        case 403 =>
          RestAuthFailure(resp.status, resp.statusText, resp.body)
        case _ =>
          RestUnknownFailure(resp.status, resp.statusText, resp.body)
      }
    }
  }

  def removeConfig(configName: String): Future[RemoveConfigRestResult] = {
    val urlPath = Settings.Loader.settings.auth.url.resolve("removeConfig")
    val auth = Settings.Loader.settings.auth

    Ws().url(urlPath).withRequestTimeout(5.minutes).addHttpHeaders(
      "Authorization" -> (auth.contractedUserId.value + "_" + auth.applicationToken.value)
    ).post(
      Json.obj(
        "configName" -> configName
      )
    ).map { resp =>
      println("status = " + resp.status)
      println("statusText = " + resp.statusText)
      resp.status match {
        case 200 =>
          val result: JsValue = Json.parse(resp.bodyAsBytes.toArray)
          RemoveConfigResultOk(
            (result \ "deleteCount").as[Int]
          )
        case 403 =>
          RestAuthFailure(resp.status, resp.statusText, resp.body)
        case _ =>
          RestUnknownFailure(resp.status, resp.statusText, resp.body)
      }
    }
  }

  private def reflect(result: JsValue) {
    println("Open config: " + result)
    import AbsoluteFieldImpl.absoluteFieldFormat

    def cropField[T](config: JsValue, ctor: ((Double, Double), Rectangle2D) => T): T = {
      val originalSize: Seq[Double] = (config \ "originalSize").as[Seq[Double]]
      val x: Double = (config \ "x").as[Double]
      val y: Double = (config \ "y").as[Double]
      val w: Double = (config \ "w").as[Double]
      val h: Double = (config \ "h").as[Double]
      ctor(
        (originalSize(0), originalSize(1)),
        new Rectangle2D(x, y, w, h)
      )
    }

    deselectAllFields()

    val record = (result \ "record").as[JsValue]
    val config: JsValue = Json.parse((record \ "config").as[String])
    (config \ "leftCropField").asOpt[JsValue].map(cropField(_, LeftCropFieldImpl.apply)).foreach {
      addLeftCropField(_, selected = false, redraw = false)
    }
    (config \ "topCropField").asOpt[JsValue].map(cropField(_, TopCropFieldImpl.apply)).foreach {
      addTopCropField(_, selected = false, redraw = false)
    }
    (config \ "rightCropField").asOpt[JsValue].map(cropField(_, RightCropFieldImpl.apply)).foreach {
      addRightCropField(_, selected = false, redraw = false)
    }
    (config \ "bottomCropField").asOpt[JsValue].map(cropField(_, BottomCropFieldImpl.apply)).foreach {
      addBottomCropField(_, selected = false, redraw = false)
    }

    (config \ "dotRemoval").asOpt[JsValue].foreach { dr =>
      val enabled = (dr \ "enabled").as[Boolean]
      if (enabled) {
        val cond = (dr \ "condition").as[DotRemovalConditionImpl]
        dotRemoval = DotRemoval(enabled, cond)
      }
    }

    (config \ "cropRectangle").asOpt[JsValue].foreach { cr =>
      val enabled = (cr \ "enabled").as[Boolean]
      if (enabled) {
        val cond = (cr \ "condition").as[CropRectangleConditionImpl]
        cropRectangle = CropRectangle(enabled, cond)
      }
    }

    (config \ "absoluteFields").as[Seq[AbsoluteField]].foreach {
      addAbsoluteField(_, isSelected = false, redraw = false)
    }

    cropEnabled = (config \ "isCropEnabled").as[Boolean]
    skewCorrection = (config \ "skewCorrection").as[SkewCorrection]

    _isDirty = false
    flushCachedImage()
  }

  def openConfig(configName: String): Future[OpenConfigRestResult] = {
    val urlPath = Settings.Loader.settings.auth.url.resolve("loadConfig")
    val auth = Settings.Loader.settings.auth

    Ws().url(urlPath).withRequestTimeout(5.minutes).addHttpHeaders(
      "Authorization" -> (auth.contractedUserId.value + "_" + auth.applicationToken.value)
    ).post(
      Json.obj(
        "configName" -> configName
      )
    ).map { resp =>
      println("status = " + resp.status)
      println("statusText = " + resp.statusText)
      resp.status match {
        case 200 =>
          val result: JsValue = Json.parse(resp.bodyAsBytes.toArray)
          Platform.runLater(() -> {
            reflect(result)
          })
          OpenConfigResultOk.parse(result)
        case 403 =>
          RestAuthFailure(resp.status, resp.statusText, resp.body)
        case _ =>
          RestUnknownFailure(resp.status, resp.statusText, resp.body)
      }
    }
  }
}
