package plotutil

import basicdef._
import util._

import scala.collection.mutable.ListBuffer
import sys.process._

def getHtmlLines(title: String, graphJson: Seq[String], extraElement: String = "") =

  val l = ListBuffer[String]()
  l += s"<!DOCTYPE html>\n<html>\n<head>\n<meta charset='UTF-8'>\n<title>$title</title>"
  l += s"<script src='http://d3js.org/d3.v3.min.js'></script>"
  l += s"<script src='https://cdn.plot.ly/plotly-2.11.1.min.js'></script>"
  l += s"</head>\n<body>\n"
  l += "<div id=chart1 tabIndex=0></div>"
  l += extraElement
  l += s"<script>\n"
  l ++= graphJson
  l += s"</script>\n"
  l += s"</body>\n</html>"
  l.toArray

def openInBrowser(
    fpOrURL: String,
    layout: RectLayout = defLayout,
    newWindow: Boolean = true,
    browser: Browser = Browser.FIREFOX,
    profile: String = "augment",
    isFile: Boolean = true
) =

  val pwd = System.getProperty("user.dir")
  val osName = sys.props.get("os.name")
  val windowSt = if newWindow then "--new-window" else ""
  val os = getOS()
  val addr = if isFile then s"file:$pwd/$fpOrURL" else fpOrURL

  val cmd = os match
    case OS.MAC =>
      Seq("open", addr)
    case OS.WINDOWS =>
      Seq("powershell", "start", addr)
    case OS.LINUX =>
      browser match
        case Browser.FIREFOX =>
          Seq(
            "firefox",
            windowSt,
            addr,
            s"--top ${layout.top}",
            s"--left ${layout.left}",
            s"--width ${layout.width}",
            s"--height ${layout.height}",
            s"-P $profile"
          )
        case Browser.CHROMIUM =>
          Seq(
            "chromium",
            windowSt,
            s"--window-position=${layout.top},${layout.left}",
            s"--window-size=${layout.width},${layout.height}",
            addr
          )
        case Browser.XDGOPEN =>
          Seq("xdg-open", addr)
    case other =>
      Seq()

  if cmd.length > 0 then
    val cmdLine = cmd.mkString(" ")
    println(s"Executing: $cmdLine")
    Process(cmdLine).lazyLines
  else Console.err.println(s"Not opened: $addr")

object imageSrcs:
  val chessImageSrcs = (
    URL("https://upload.wikimedia.org/wikipedia/commons/a/af/Chess_qdt60.png"),
    URL("https://upload.wikimedia.org/wikipedia/commons/d/d7/Chessboard480.svg")
  )
