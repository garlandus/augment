package multiarrayplot

import basicdef._
import multiarray._
import plotutil._
import shape._
import util._
import util.JavaUtil._
import util.StringExtensions._

import collection.JavaConverters._
import math.Numeric.Implicits.infixNumericOps
import scala.collection.mutable.Set as MSet
import scala.reflect.ClassTag

val plotFldr = "out/plots"

trait PlotShape:
  def `type`: String
  def x0: Float
  def y0: Float
  def x1: Float
  def y1: Float
  def line: Line

trait RectangleTr extends PlotShape:
  override val `type`: String = "rect"

case class Rectangle(x0: Float, y0: Float, x1: Float, y1: Float, line: Line) extends RectangleTr

case class FilledRectangle(
    x0: Float,
    y0: Float,
    x1: Float,
    y1: Float,
    line: Line,
    fillcolor: Color | String | Verbatim | ImageSpec
) extends RectangleTr

object PlotExtensionsA:

  extension [X, A](a: => MultiArrayA[X, A])(using ClassTag[X])

    def toJS(): String =
      val xs = s"x = ${toJSON(a.as)}"
      val ys = s"y = ${toJSON(a.flat())}"
      val ys2 = s"var data_y = {x: x, y: y, type: 'line', name: 'X'}"
      val s = "Plotly.newPlot('chart1', [ data_y ])"
      s"\n$xs\n$ys\n$ys2\n$s\n"

    def toHTML(title: String): Seq[String] =
      val data = toJS().split("\n")
      getHtmlLines(title, data)

    def plot(title: String = "", addTimeStamp: Boolean = false, name: String = "") =
      val fileName = getFileName(title, addTimeStamp, name, "plotA")
      saveToFile(plotFldr, fileName, a.toHTML(title).mkString("\n"), "html")
      openInBrowser(s"$plotFldr/$fileName")

object PlotExtensionsB:

  extension [X, A, B](arr: => MultiArrayB[X, A, B])(using ClassTag[X])

    def axesToJS(): (String, String) =
      val xs = s"x = ${toJSON(arr.bs.toList)}"
      val yNames = arr.as.head match
        case x: Named[_] => arr.as.map(a => a.asInstanceOf[Named[_]].name).toList
                case _           => arr.as.toList
      val ys = s"y = ${toJSON(yNames)}"
      (xs, ys)

    def toJS(): String =
      val (xs, ys) = axesToJS()
      val zs = s"z = ${toJSON(arr.nested())}"
      s"\n$xs\n$ys\n$zs\n"

    def toHTML(title: String): Seq[String] =
      val js = toJS()
      val data = js.split("\n")
      val s2 = "var data_z = {x: x, y: y, z: z, type: 'surface'}"
      val s3 = """Plotly.newPlot('chart1', [data_z])"""
      getHtmlLines(title, data ++ List(s2, s3))

    def toCoords(falseAsX: X): Seq[(Int, Int)] =
      arr.as
        .map: a =>
          arr.bs.map: b =>
            val x = arr.apply(a, b)
            val ia = arr.as.indexOf(a)
            val ib = arr.bs.indexOf(b)
            if (x == falseAsX) then None else Some((ib, ia))
        .flatten
        .flatten
        .toArray

    def asRectangles(
        falseAsX: X,
        color: String,
        getGridColors: (Int, Int) => Color,
        useImages: Boolean,
        usedCoords: MSet[(A, B)] = MSet[(A, B)]()
    ): Array[FilledRectangle] =
      val line = Line("white", 0)

      val rectangles: Array[FilledRectangle] =
        arr.as
          .map: a =>
            arr.bs.map: b =>
              val x = arr.apply(a, b)
              val ia = arr.as.indexOf(a)
              val ib = arr.bs.indexOf(b)
              val emptySquareColor = getGridColors(ia, ib).color.toString

              if (x == falseAsX) then
                if useImages || !usedCoords.contains((a, b)) then None
                else Some(FilledRectangle(ib, ia, ib + 1, ia + 1, Line("white", 0), emptySquareColor))
              else
                usedCoords += ((a, b))
                Some(FilledRectangle(ib, ia, ib + 1, ia + 1, line, color))
          .flatten
          .flatten
          .toArray
      rectangles

    def rectsToJson(
        falseAsX: X,
        color: String,
        useImages: Boolean,
        getGridColors: (Int, Int) => Color = (_, _) => Color("#FFFFFF"),
        usedCoords: MSet[(A, B)] = MSet[(A, B)]()
    ): String =
      val rects = arr.asRectangles(falseAsX, color, getGridColors, useImages, usedCoords)
      rectsAsJson(rects)

    def toJsFlat(): String =
      val xs = s"x = ${toJSON(arr.bs)}"
      val inds = 0 to arr.as.length - 1
      val ys = inds.map(i => s"y$i = ${toJSON(arr.nested()(i).toArray)}")
      s"\n$xs\n${ys.mkString("\n")}\n"

    def toHtmlFlat(title: String): Seq[String] =
      val js = toJsFlat()
      val data = js.split("\n")
      val yNames = arr.as.head match
        case x: Named[_] => arr.as.map(_.asInstanceOf[Named[_]].name)
        case _           => arr.as

      val inds = 0 to arr.as.length - 1
      val sts = inds.map(i => s"var data$i = {x: x, y: y$i, type: 'line'}")
      val curveNms = inds.map(i => s"data$i").mkString(", ")

      val m = inds.map(i => Map("x" -> Verbatim("x"), "y" -> Verbatim(s"y$i"), "type" -> "line", "name" -> yNames(i)))
      val s = toJSON(m)
      val s1 = s"var data = ${toJSON(m)}"
      val s2 = s"""Plotly.newPlot('chart1', data)"""
      getHtmlLines(title, data ++ List(s1) ++ List(s2))

    def plot(title: String = "", addTimeStamp: Boolean = false, name: String = "", visibleAxes: Boolean) =
      val fileName = getFileName(title, addTimeStamp, name, "plotB")
      saveToFile(plotFldr, fileName, arr.toHTML(title).mkString("\n"), "html")
      openInBrowser(s"$plotFldr/$fileName")

    def plotflat(title: String = "", addTimeStamp: Boolean = false, name: String = "", visibleAxes: Boolean) =
      val fileName = getFileName(title, addTimeStamp, name, "flat")
      saveToFile(plotFldr, fileName, arr.toHtmlFlat(title).mkString("\n"), "html")
      openInBrowser(s"$plotFldr/$fileName")

def rectsAsJson(rects: Array[FilledRectangle]): String =
  val entries: Array[Any] = rects.map(r => Array(r.x0, r.y0, r.x1, r.y1, r.line.color, r.fillcolor))
  toJSON(entries).toString.replaceAll("\\[", "\n\t\\[").replaceAll("\"", "'")

def frameAdder(n: Int, divNm: String, frameGrid: String) =
  s"""
    let inds = [...Array($n).keys()]
    let all_frames = inds.map(i => `frame$${i}`)

    function getFrame(i) {return {data: all_data [i], name: all_frames [i]}}
    function getFrameGrid(i) {return $frameGrid}

    function getFrameImage(i)
    {
      return {data: [], name: all_frames[i], layout: {images: images[i]}}
    }

    function addFrames(f, inds)
    {
      let frames = inds.map(f)
      Plotly.addFrames('$divNm', frames)
    }
  """.stripMrgn

def animationStarter(duration: Duration) =
  val durationsSt = duration match
    case i: Int       => toJSON(Map("duration" -> i))
    case d: Durations => toJSON(d)
  s"""
      function startAnimation()
      {
        Plotly.animate('chart1', all_frames,
        {
          frame: $durationsSt,
          transition: [{duration: 500, easing: 'elastic-in'}],
          mode: 'afterall'
        })
      }
    """.stripMrgn

def plotter(animate: Boolean, flat: Boolean, useImages: Boolean, divNm: String, initialData: String, layoutSt: String) =
  (animate, flat, useImages) match
    case (false, _, _) =>
      s"""Plotly.newPlot('$divNm', all_data)"""
    case (true, false, _) =>
      s"""Plotly.newPlot('$divNm', [all_data.at(-1)])\n  .then (addFrames(getFrame, inds))\n  .then (startAnimation)"""
    case (true, true, false) =>
      s"""Plotly.newPlot('$divNm', $initialData, $layoutSt)\n  .then (addFrames(getFrameGrid, inds))\n  .then (startAnimation)"""
    case (true, true, true) =>
      s"""Plotly.newPlot('$divNm', $initialData, $layoutSt)\n  .then (addFrames(getFrameImage, inds))\n  .then (startAnimation)"""

object PlotExtensionsC:

  extension [X, A, B, C](arr: => MultiArrayC[X, A, B, C])(using ClassTag[X])

    def toJS(
        animate: Boolean,
        flat: Boolean,
        imageSources: Option[(URL, URL)],
        duration: Duration,
        visibleAxes: Boolean,
        getGridColors: (Int, Int) => Color
    ): String =
      val usedCoords = MSet[(B, C)]()

      val useImages = imageSources != None
      val xs = s"x = ${toJSON(arr.cs)}"
      val ys = s"y = ${toJSON(arr.bs)}"
      val arrays = arr.nested().map(_.map(_.toArray).toArray).toArray
      val divNm = "chart1"
      val (width, height) = (500, 520)
      val z1s = "z = " + toJSON(arrays)

      val inds = (0 until arrays.length)
      val traceNms = inds.map(i => s"Trace Nm $i...").toArray
      val colorScaleVals = "Jet,Picnic,Portland,Rainbow,RdBu,Reds,Viridis,YlGnBu,YlOrRd"
      val allColorScales = colorScaleVals.split(",")
      val colorScales = inds.map(i => allColorScales(i % allColorScales.length)).toArray

      val colorScSt1 = s"let colorScaleSt = `$colorScaleVals`"
      val colorScSt2 = s"let colorScales = colorScaleSt.split(',')"
      val zs = "let data_z = z.map((data, i) => " +
        s"({x: x, y: y, z: data, type: 'surface', name: `Trace $${i}`, colorscale: `$${colorScales[i % colorScales.length]}`}))\n"

      val surfaceNms = inds.map(i => s"data_z$i").mkString(", ")
      val frameNms = inds.map(i => s"'frame$i'").mkString(", ")

      val frameAdderSt = frameAdder(inds.last + 1, divNm, "{data: [], name: all_frames[i], layout: {shapes: grids[i]}}")
      val animateSt = animationStarter(duration)

      val lineClr = "rgba(128, 0, 128, 1)"
      val fillClr = "rgba(128, 0, 128, 0.7)"
      val sg1 = s"let lineClr = '$lineClr'"
      val sg2 = s"let fillClr = '$fillClr'"
      val gridSt = if flat then s"$sg1\n$sg2\n${toFlatJS(imageSources, getGridColors, usedCoords)}" else ""

      val s1 = s"var all_data = data_z"
      val xAxisLayout = AxisLayoutRange((0, arr.cs.length), "blue", visibleAxes, true, false)
      val yAxisLayout = AxisLayoutRange((0, arr.bs.length), "blue", visibleAxes, true, false)

      val layout =
        if useImages then ImagesLayout(xAxisLayout, yAxisLayout, width, height, Verbatim("[]"), Verbatim("images[0]"))
        else ImagesLayout(xAxisLayout, yAxisLayout, width, height, Verbatim("grids[0]"), Verbatim("images"))
      val layoutSt = toJSON(layout).toString

      val st = plotter(animate, flat, useImages, divNm, "[]", layoutSt)
      s"\n$xs\n$ys\n\n$z1s\n\n$colorScSt1\n$colorScSt2\n$zs\n\n$gridSt\n\n$s1\n$frameAdderSt$animateSt\n$st"

    def toFlatJS(
        imageSources: Option[(URL, URL)],
        getGridColors: (Int, Int) => Color,
        usedCoords: MSet[(B, C)]
    ): String =
      val subArrays = arr.as.map(arr.subArray)
      val color = "blue"

      val (useImages, imageSrc, backgroundImageSrc) = imageSources match
        case Some(url1, url2) => (true, url1, url2)
        case None             => (false, "N/A", "N/A")

      val rects = subArrays.zipWithIndex.map((a, i) =>
        PlotExtensionsB.asRectangles(a)(0.asInstanceOf[X], color, getGridColors, useImages, usedCoords)
      )
      val entriesSt = rects.map(rectsAsJson).mkString(",\n")

      val usedCoords_ = MSet[(B, C)]()
      val coords = subArrays.map(PlotExtensionsB.toCoords(_)(0.asInstanceOf[X]))
      val coordArr = coords.map(_.map(List(_, _).toArray).toArray).toArray
      val coordSt = toJSON(coordArr).toString

      val fns =
        s"""
        function coordsToSquare(c)
        {
          return [c[0], c[1], c[0]+1, c[1]+1, "white", c[2] ? 'blue' : 'white']
        }

        function arrayHasPair(arr, pr)
        {
          return !(arr.find(x => x[0] == pr[0] && x[1] == pr[1]) == undefined)
        }

        function coordsToMarkedAndBlank(coords)
        {
          let usedCoords = []
          return coords.map(board =>
            {
              let blSqs = usedCoords.map(c => [c[0], c[1], 0])
              let markedSquares = board.map(c =>
              {
                if (!arrayHasPair(usedCoords, c))
                  usedCoords.push(c);
                return [c[0], c[1], 1]
              })
              let blankSquares = blSqs.filter (c => !arrayHasPair(markedSquares, c))
              let boardSquares = markedSquares.concat(blankSquares)
              return boardSquares
            }
          )
        }

        function coordsToEntries(coords)
        {
          return coords.map(board => board.map(c => coordsToSquare(c)))
        }

        let fullCoords = coordsToMarkedAndBlank(allCoords)
        let entries = coordsToEntries(fullCoords)
        let useBackgroundImage = $useImages
        let imageSrc = '$imageSrc'
        let backgroundImageSrc = '$backgroundImageSrc'

        let grids = entries.map(stepEntries => stepEntries.map(t =>
          ({'type':'rect', 'x0':t[0], 'y0':t[1], 'x1':t[2], 'y1':t[3], 'line':{'color':t[4],'width':0}, 'fillcolor':t[5]})))

        let backgroundImage = {'x':0, 'y':0, 'sizex':8, 'sizey':8, 'source': backgroundImageSrc,
                'xanchor':'left','xref':'x','yanchor':'bottom','yref':'y','layer':'above'}
        let images = allCoords.map((stepEntries, i) =>
          [].concat(
            useBackgroundImage && i >= 0 ? [backgroundImage] : [],
            stepEntries.map(t =>        
              ({'x':t[0], 'y':t[1], 'sizex':1.0, 'sizey':10.0, 'source': imageSrc,
              'xanchor':'left','xref':'x','yanchor':'bottom','yref':'y','layer':'above'})))
            )
      """.stripMrgn
      s"let allCoords = $coordSt\n" + fns

    def toHTML(
        title: String,
        animate: Boolean,
        flat: Boolean,
        imageSources: Option[(URL, URL)],
        duration: Duration,
        visibleAxes: Boolean,
        getGridColors: (Int, Int) => Color = (_, _) => Color("#FFFFFF")
    ): Seq[String] =
      val js = toJS(animate, flat, imageSources, duration, visibleAxes, getGridColors)
      val data = js.split("\n") ++ List("\n")
      getHtmlLines(title, data)

    def plot(title: String = "", addTimeStamp: Boolean = false, name: String = "", visibleAxes: Boolean = true) =
      val fileName = getFileName(title, addTimeStamp, name, "plotC")
      saveToFile(plotFldr, fileName, arr.toHTML(title, false, false, None, 750, visibleAxes).mkString("\n"), "html")
      openInBrowser(s"$plotFldr/$fileName")

    def animate(
        title: String = "",
        flat: Boolean = false,
        duration: Duration,
        addTimeStamp: Boolean,
        name: String = "",
        visibleAxes: Boolean = true,
        imageSources: Option[(URL, URL)],
        getGridColors: (Int, Int) => Color
    ): Unit =
      val fileName = getFileName(title, addTimeStamp, name, "animateC")
      saveToFile(
        plotFldr,
        fileName,
        arr.toHTML(title, true, flat, imageSources, duration, visibleAxes, getGridColors).mkString("\n"),
        "html"
      )
      openInBrowser(s"$plotFldr/$fileName")

val indentSt = "\t"

case class RelOffset(lineDepth: Int, indentLevel: Int):
  def add(pr: (Int, Int)) = RelOffset(lineDepth + pr._1, indentLevel + pr._2)
  def add(ro: RelOffset) = RelOffset(lineDepth + ro.lineDepth, indentLevel + ro.indentLevel)
  override def toString(): String =
    if lineDepth > 0 || indentLevel > 0 then s"  $lineDepth . $indentLevel  " else ""

def sum(offset: Seq[RelOffset]): RelOffset =
  offset.toList match
    case h :: t => h.add(sum(t))
    case Nil    => flush

case class TextNode(
    ro: RelOffset,
    s: String,
    subNodes: Array[TextNode] = Array(),
    areSubNodesStatements: Boolean = false
):

  override def toString(): String = toSt(flush)

  def toSt(offsetFromOrigin: RelOffset): String =
    val fullOffset = offsetFromOrigin.add(ro)
    val offsetSt = "\n" * ro.lineDepth + indentSt * fullOffset.indentLevel
    val subNodeSeparator = if areSubNodesStatements then "\n" else ","
    s"$offsetSt$s${subNodes.map(_.toSt(fullOffset)).mkString(subNodeSeparator).replace("[,", "[").replace(",]", "]").replace(":,", ":").replace("{,", "{").replace(",}", "}")}"

  def maxOffset(): RelOffset =
    subNodes match
      case Array() => RelOffset(0, s.length)
      case _ =>
        val offsets = subNodes.map(_.maxOffset()).toList
        sum(offsets)

val flush = RelOffset(0, 0)

def toJsExpr[A](expr: A, ro: RelOffset = RelOffset(0, 0)): TextNode =
  expr match
    case a: Assignment[_] =>
      val s = s"let ${a.varName} = ${toJSON(a.value)}"
      TextNode(ro, s)
    case JsFunction(name, varNames, body) =>
      val s = s"function $name (${varNames.mkString(", ")}) {\n"
      val subNodes = body.map(x => toJsExpr(x, RelOffset(0, 1))) ++ List(TextNode(ro, "}\n"))
      TextNode(ro, s, subNodes.toArray, true)
    case ReturnStatement(varName) =>
      TextNode(ro, s"return $varName")
    case _ => throw new Exception(s"Case not found for expr: [$expr]")

def toJSON[A](a: A, ro: RelOffset = RelOffset(0, 0)): TextNode =
  a match
    case x: (Int | Float | Double) => TextNode(ro, x.toString)
    case b: Boolean                => TextNode(ro, b.toString)
    case c: Char                   => TextNode(ro, s"\'$c\'")
    case s: String                 => TextNode(ro, s"\"$s\"")
    case Verbatim(s: String)       => TextNode(ro, s)
    case l: Seq[_] =>
      val x = l.map(toJSON(_, flush))
      val m = sum(x.map(_.maxOffset()))
      val offsets =
        if m.lineDepth == 0 && m.indentLevel < 20 then (flush, flush, flush)
        else if m.lineDepth == 0 && m.indentLevel < 150 then (RelOffset(1, 1), RelOffset(2, 1), RelOffset(0, 0))
        else (RelOffset(1, 0), RelOffset(2, 1), RelOffset(1, 0))

      val tns = List(TextNode(offsets._1, "[")) ++ x ++ List(TextNode(offsets._3, "]"))
      TextNode(ro, s"", tns.toArray)

    case a: Array[_] =>
      toJSON(a.toList, ro)
    case c: Color =>
      TextNode(ro, s"\"${c.toString}\"")
    case Durations(d) =>
      d match
        case (i: Int) :: t =>
          val l = d.asInstanceOf[List[Int]]
          toJSON(l.map(x => Map("duration" -> x)))
        case (pr: (Int, Int)) :: t =>
          val l0 = d.asInstanceOf[List[(Int, Int)]]
          val l = l0.map((d, n) => List.fill(n)(d)).flatten
          toJSON(l.map(x => Map("duration" -> x)))
        case Nil => toJSON(List())

    case (a, b) =>
      val (x, y) = (toJSON(Verbatim(a.toString)), toJSON(b))
      TextNode(x.ro, x.s, Array(TextNode(flush, ":"), y))

    case p: Product =>
      toJSON((p.productElementNames zip p.productIterator).toMap)
    case m: Map[_, _] =>
      val tn = toJSON(m.toList)
      val brackets = (tn.subNodes.head, tn.subNodes.last)
      val mapBrackets = (TextNode(tn.subNodes.head.ro, "{"), TextNode(tn.subNodes.head.ro, "}"))
      TextNode(tn.ro, tn.s, Array(mapBrackets._1) ++ tn.subNodes.drop(1).dropRight(1) ++ List((mapBrackets._2)))
    case _ => throw new Exception(s"Case not found: [$a]")

object PlotExtensionsSeq:

  extension [A](a: Seq[(A, A)])(using Numeric[A], ClassTag[A])

    def toJSB(
        title: String,
        sphereSize: Int,
        duration: Duration,
        visibleAxes: Boolean,
        color: Color,
        animate: Boolean
    ): String =
      val (xs, ys) = (a.map(_._1).toList, a.map(_._2).toList)
      val setXsSt = toJsExpr(Assignment("x", xs))
      val setYsSt = toJsExpr(Assignment("y", ys))

      val line = Line(Color("#003399"), 3)
      val marker = Marker(sphereSize, color, line)
      val setMarkerSt = toJsExpr(Assignment("marker", marker))

      val trace = Trace(Verbatim("x"), Verbatim("y"), "markers", Verbatim("marker"), "scatter")
      val setTraceSt = toJsExpr(Assignment("trace", trace))

      val trace1 =
        Trace(Verbatim("x.slice(0, n)"), Verbatim("y.slice(0, n)"), "markers", Verbatim("marker"), "scatter")
      val setTraceSt1 = Assignment("res", trace1)
      val tracerFn = JsFunction("tracer", Seq("n"), List(setTraceSt1, ReturnStatement("res")))
      val tracerFnSt = toJsExpr(tracerFn)

      val margin = Numeric[A].one
      val (maxX, maxY) = (xs.max + margin, ys.max + margin)
      val (xAxisLayout, yAxisLayout) =
        (
          AxisLayoutRange((Numeric[A].zero, maxX), "blue", visibleAxes, true, false),
          AxisLayoutRange((Numeric[A].zero, maxY), "blue", visibleAxes, true, false)
        )

      val layout = PlotLayoutFlat(title, 580, 600, xAxisLayout, yAxisLayout, Margin(100, 100, 150, 100, 25), true)
      val setLayoutSt = toJsExpr(Assignment("layout", layout))

      val traceSt =
        s"""
          $setXsSt
          $setYsSt
          $setLayoutSt

          $setMarkerSt
          $setTraceSt

          $tracerFnSt

          var all_data = {data: [trace], layout: layout}
          var grids = []
        """.stripMrgn

      val frameAdderSt = frameAdder(a.length + 1, "chart1", "{data: [tracer(i)], name: 'frame' + i}")
      val animateSt = animationStarter(duration)

      val st = plotter(animate, true, false, "chart1", "[tracer(0)]", "layout")
      val s1 = if animate then s"$frameAdderSt$animateSt" else ""
      s"\n$traceSt\n$s1\n$st\n"

    def toHTMLB(
        title: String,
        sphereSize: Int,
        duration: Duration,
        visibleAxes: Boolean,
        color: Color,
        animate: Boolean = false
    ): Seq[String] =
      val js = toJSB(title, sphereSize, duration, visibleAxes, color, animate)
      val data = js.split("\n")
      getHtmlLines(title, data)

    def plotB(
        title: String = "",
        sphereSize: Int = 10,
        duration: Duration = 150,
        visibleAxes: Boolean = true,
        color: Color = mayaBlue,
        addTimeStamp: Boolean = false,
        name: String = ""
    ) =
      val fileName = getFileName(title, addTimeStamp, name, "plotSeqB")
      saveToFile(plotFldr, fileName, a.toHTMLB(title, sphereSize, duration, visibleAxes, color).mkString("\n"), "html")
      openInBrowser(s"$plotFldr/$fileName")

    def animateB(
        title: String = "",
        sphereSize: Int = 10,
        duration: Duration = 150,
        visibleAxes: Boolean = true,
        color: Color = mayaBlue,
        addTimeStamp: Boolean = false,
        name: String = ""
    ) =
      val fileName = getFileName(title, addTimeStamp, name, "animateSeqB")
      saveToFile(
        plotFldr,
        fileName,
        a.toHTMLB(title, sphereSize, duration, visibleAxes, color, true).mkString("\n"),
        "html"
      )
      openInBrowser(s"$plotFldr/$fileName")

  extension [A](a: Seq[(A, A, A)])(using Numeric[A], ClassTag[A])

    def toJS(
        title: String,
        sphereSize: Int,
        duration: Duration,
        visibleAxes: Boolean,
        color: Color,
        animate: Boolean
    ): String =
      val (zs, ys, xs) = (a.map(_._1).toList, a.map(_._2).toList, a.map(_._3).toList)
      val setXsSt = toJsExpr(Assignment("x", xs))
      val setYsSt = toJsExpr(Assignment("y", ys))
      val setZsSt = toJsExpr(Assignment("z", zs))

      val line = Line(Color("#003399"), 3)
      val marker = Marker(sphereSize, color, line)
      val setMarkerSt = toJsExpr(Assignment("marker", marker))

      val trace = Trace3D(Verbatim("x"), Verbatim("y"), Verbatim("z"), "markers", Verbatim("marker"), "scatter3d")
      val setTraceSt = toJsExpr(Assignment("trace", trace))

      val trace1 = Trace3D(
        Verbatim("x.slice(0, n)"),
        Verbatim("y.slice(0, n)"),
        Verbatim("z.slice(0, n)"),
        "markers",
        Verbatim("marker"),
        "scatter3d"
      )
      val setTraceSt1 = Assignment("res", trace1)
      val tracerFn = JsFunction("tracer", Seq("n"), List(setTraceSt1, ReturnStatement("res")))
      val tracerFnSt = toJsExpr(tracerFn)

      val axisMargin = Numeric[A].one
      val (minX, maxX, minY, maxY, minZ, maxZ) =
        (
          xs.min - axisMargin,
          xs.max + axisMargin,
          ys.min - axisMargin,
          ys.max + axisMargin,
          zs.min - axisMargin,
          zs.max + axisMargin
        )
      val (xAxisLayout, yAxisLayout, zAxisLayout) =
        (
          AxisLayoutRange((minX, maxX), "blue", visibleAxes, true, false),
          AxisLayoutRange((minY, maxY), "blue", visibleAxes, true, false),
          AxisLayoutRange((minZ, maxZ), "blue", visibleAxes, true, false)
        )

      val camera = Camera(Coords3D(-1.75, -1.75, 1.25), Coords3D(0, 0, 1), Coords3D(0, 0, 0))
      val layout = PlotLayoutVol(
        title,
        700,
        700,
        AxisLayoutsVol(xAxisLayout, yAxisLayout, zAxisLayout, camera),
        Margin(100, 100, 150, 100, 25)
      )
      val setLayoutSt = toJsExpr(Assignment("layout", layout))

      val traceSt =
        s"""
          $setXsSt
          $setYsSt
          $setZsSt
          $setLayoutSt

          $setMarkerSt
          $setTraceSt

          $tracerFnSt

          var all_data = {data: [trace], layout: layout}
        """.stripMrgn

      val frameAdderSt = frameAdder(a.length + 1, "chart1", "{data: [tracer(i)], name: 'frame' + i}")
      val animateSt = animationStarter(duration)

      val st = plotter(animate, true, false, "chart1", "[tracer(0)]", "layout")
      val s1 = if animate then s"$frameAdderSt$animateSt" else ""
      s"\n$traceSt\n$s1\n$st\n"

    def toHTML(
        title: String,
        sphereSize: Int,
        duration: Duration,
        visibleAxes: Boolean = true,
        color: Color = mayaBlue,
        animate: Boolean = false
    ): Seq[String] =
      val js = toJS(title, sphereSize, duration, visibleAxes, color, animate)
      val data = js.split("\n")
      getHtmlLines(title, data)

    def plot(
        title: String = "",
        sphereSize: Int = 10,
        duration: Duration = 150,
        visibleAxes: Boolean = true,
        color: Color = mayaBlue,
        addTimeStamp: Boolean = false,
        name: String = ""
    ) =
      val fileName = getFileName(title, addTimeStamp, name, "plotSeq")
      saveToFile(plotFldr, fileName, a.toHTML(title, sphereSize, duration, visibleAxes, color).mkString("\n"), "html")
      openInBrowser(s"$plotFldr/$fileName")

    def animate(
        title: String = "",
        sphereSize: Int = 10,
        duration: Duration = 150,
        visibleAxes: Boolean = true,
        color: Color = mayaBlue,
        addTimeStamp: Boolean = false,
        name: String = ""
    ) =
      val fileName = getFileName(title, addTimeStamp, name, "animateSeq")
      saveToFile(
        plotFldr,
        fileName,
        a.toHTML(title, sphereSize, duration, visibleAxes, color, true).mkString("\n"),
        "html"
      )
      openInBrowser(s"$plotFldr/$fileName")

  extension [A](a: JList[(JInteger, JInteger, JInteger)])
    def plot(
        title: String,
        sphereSize: Int,
        duration: Duration,
        color: Color,
        visibleAxes: Boolean,
        addTimeStamp: Boolean
    ): Unit =
      a.asScala.toList
        .map(t => (t._1.toInt, t._2.toInt, t._3.toInt))
        .plot(title, sphereSize, duration, visibleAxes, color, addTimeStamp)

  extension (a: JList[Pair[JInteger, JInteger]])

    def plotPairs(
        title: String,
        sphereSize: Int,
        duration: Duration,
        visibleAxes: Boolean,
        color: Color,
        addTimeStamp: Boolean
    ): Unit =
      a.asScala.toList
        .map(t => (t.first.toInt, t.second.toInt))
        .plotB(title, sphereSize, duration, visibleAxes, color, addTimeStamp)

    def animatePairs(
        title: String,
        sphereSize: Int,
        duration: Duration,
        visibleAxes: Boolean,
        color: Color,
        addTimeStamp: Boolean
    ): Unit =
      a.asScala.toList
        .map(t => (t.first.toInt, t.second.toInt))
        .animateB(title, sphereSize, duration, visibleAxes, color, addTimeStamp)

  extension (a: JList[Triple[JInteger, JInteger, JInteger]])

    def plotTriples(
        title: String,
        sphereSize: Int,
        duration: Duration,
        visibleAxes: Boolean,
        color: Color,
        addTimeStamp: Boolean
    ): Unit =
      a.asScala.toList
        .map(t => (t.first.toInt, t.second.toInt, t.third.toInt))
        .plot(title, sphereSize, duration, visibleAxes, color, addTimeStamp)

    def animateTriples(
        title: String,
        sphereSize: Int,
        duration: Duration,
        visibleAxes: Boolean,
        color: Color,
        addTimeStamp: Boolean
    ): Unit =
      a.asScala.toList
        .map(t => (t.first.toInt, t.second.toInt, t.third.toInt))
        .animate(title, sphereSize, duration, visibleAxes, color, addTimeStamp)
