package basicdef

import multiarrayplot.FilledRectangle

import scala.reflect.ClassTag

case class Margin(l: Int, r: Int, b: Int, t: Int, pad: Int)
case class AxisLayoutBasic(title: String, color: String)
case class AxisLayoutA(title: String, color: String, nticks: Int, tickformat: String)
case class AxisLayoutB(title: String, color: String, tick0: Int, dtick: Int, tickformat: String)
case class AxisLayoutDates(title: String, color: String, rangebreaks: Seq[RangeBreak], `type`: String = "date")

case class AxisLayoutRange[A](range: Array[A], color: String, visible: Boolean = true, showgrid: Boolean = true)
object AxisLayoutRange:
  def apply[A](range: (A, A), color: String, visible: Boolean, showgrid: Boolean, zeroline: Boolean)(using
      ClassTag[A]
  ): AxisLayoutRange[A] =
    AxisLayoutRange(Array[A](range._1, range._2), color, visible, showgrid)

case class AxisLayoutsFlat(xaxis: AxisLayout, yaxis: AxisLayout)
case class AxisLayoutsVol(xaxis: AxisLayout, yaxis: AxisLayout, zaxis: AxisLayout, camera: Camera = Camera())
case class RangeBreak(bounds: List[Float | String], pattern: String = "")
case class Scene(camera: Camera = Camera())

type AxisLayout = AxisLayoutBasic | AxisLayoutA | AxisLayoutB | AxisLayoutDates | AxisLayoutRange[_]
type AxisLayouts = AxisLayoutsFlat | AxisLayoutsVol

case class ImageSpec(source: String, sizex: Float, sizey: Float)
case class Image(
    x: Float,
    y: Float,
    sizex: Float,
    sizey: Float,
    source: String,
    xanchor: String = "left",
    xref: String = "x",
    yanchor: String = "bottom",
    yref: String = "y",
    layer: String = "above"
)

case class ShapesLayout(
    xaxis: AxisLayout,
    yaxis: AxisLayout,
    width: Int,
    height: Int,
    shapes: Array[FilledRectangle] | String
)
case class ImagesLayout(
    xaxis: AxisLayout,
    yaxis: AxisLayout,
    width: Int,
    height: Int,
    shapes: Array[FilledRectangle] | Verbatim,
    images: Array[Image] | Verbatim
)

case class PlotLayoutFlat(
    title: String,
    width: Int,
    height: Int,
    xaxis: AxisLayout,
    yaxis: AxisLayout,
    margin: Margin,
    showlegend: Boolean,
    plot_bgcolor: Color = plotBgColor,
    paper_bgcolor: Color = paperBgColor
)
case class PlotLayoutVol(
    title: String,
    width: Int,
    height: Int,
    scene: AxisLayoutsVol,
    margin: Margin,
    showlegend: Boolean = true,
    plot_bgcolor: Color = plotBgColor,
    paper_bgcolor: Color = paperBgColor
)
type PlotLayout = PlotLayoutFlat | PlotLayoutVol

case class RectLayout(top: Int, left: Int, width: Int, height: Int)
val defLayout = RectLayout(10, 10, 1300, 750)

case class Coords3D(x: Float = 0, y: Float = 0, z: Float = 0)
case class Camera(
    eye: Coords3D = Coords3D(1.25, 1.25, 1.25),
    up: Coords3D = Coords3D(z = 0.1),
    center: Coords3D = Coords3D()
)

case class HoverLabel(
    bgcolor: String = "rgba (0, 0, 255, .75)",
    font_size: Int = 14,
    font_family: String = "arial",
    hovermode: String = "x unified"
)
case class Font(size: Int = 14, family: String = "arial", color: String = "black")

case class Color(color: String | ColorRGBA):
  override def toString(): String = color.toString
case class ColorRGBA(r: Int, g: Int, b: Int, a: Int = 1):
  override def toString(): String = s"rgba($r,$g,$b,$a)"

val emptyColor = Color("")
val plotBgColor = Color("#e6e6e6")
val paperBgColor = Color("#ffffff")
val mayaBlue = Color("#66CCFF")

case class Line(color: Color | String = emptyColor, width: Int = 2)

case class Durations(durations: List[Int | (Int, Int)])
type Duration = Int | Durations
case class Verbatim(s: String)

case class Marker(size: Int, color: Color, line: Line)
case class Trace[A, B](
    x: Seq[A] | Verbatim,
    y: Seq[B] | Verbatim,
    mode: String,
    marker: Marker | Verbatim,
    `type`: String
)
case class Trace3D[A, B, C](
    x: Seq[A] | Verbatim,
    y: Seq[B] | Verbatim,
    z: Seq[C] | Verbatim,
    mode: String,
    marker: Marker | Verbatim,
    `type`: String
)

case class Assignment[A](varName: String, value: A, local: Boolean = true)
case class ReturnStatement(varName: String)
case class JsFunction(name: String, argNames: Seq[String], body: Seq[Any])
