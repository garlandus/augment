package util

import basicdef._
import multiarray._

import java.io.{File, FileWriter, BufferedWriter}
import java.time.LocalDateTime
import java.time.format.DateTimeFormatter
import java.util.concurrent.ConcurrentLinkedDeque
import java.util.Spliterators
import java.util.stream.{BaseStream, IntStream, StreamSupport}
import scala.collection.JavaConverters._
import scala.collection.mutable.Set
import scala.reflect.ClassTag

enum OS:
  case LINUX, MAC, WINDOWS, OTHER

enum Browser:
  case CHROMIUM, FIREFOX, XDGOPEN

var foldersChecked = Set[String]()
val rand = new scala.util.Random

case class FilePath(folder: String, fileNm: String):
  override def toString(): String = s"$folder/$fileNm"

def timed[A](f: => A): (A, Float) =
  val t1 = System.nanoTime
  val res = f
  val t2 = System.nanoTime
  val duration = t2 - t1
  (res, duration / 1000f)

def getFilePath(folder: String, fileNm: String) =
  if !foldersChecked.contains(folder) then
    newFolderIfMissing(folder)
    foldersChecked.add(folder)
  val pfx = if (folder.startsWith("/") || folder == ".") then "" else s"./"
  val fp = FilePath(s"$pfx$folder", fileNm)
  fp

def saveToFile(fp: FilePath, s: String, description: String): Unit =
  saveToFile(fp.folder, fp.fileNm, s, description)

def saveToFile(folder: String, fileNm: String, s: String, desc: String = ""): Unit =
  val fp = getFilePath(folder, fileNm)
  println(s"Saving ${if desc.length > 0 then desc + " " else ""}to $fp ...")
  val f = new File(fp.toString)
  val bw = new BufferedWriter(new FileWriter(f))
  bw.write(s)
  bw.close()

def saveToFile(folder: String, fileNm: String, l: Seq[String], description: String): Unit =
  val fp = getFilePath(folder, fileNm)
  val s = l.mkString("\n")
  saveToFile(fp, s, description)

def newFolderIfMissing(fp: String): Boolean =
  if !File(fp).exists then
    println(s"Adding new folder: $fp")
    File(fp).mkdir
  else true

def getOS(): OS =
  val osName = sys.props.get("os.name")
  osName.map(_.toLowerCase) match
    case Some(w) if w.startsWith("windows") => OS.WINDOWS
    case Some("mac os x")                   => OS.MAC
    case Some(l) if l.indexOf("linux") >= 0 => OS.LINUX
    case _                                  => OS.OTHER

def timeSt() =
  DateTimeFormatter.ofPattern("MM.dd_HH.mm").format(LocalDateTime.now)

def floatToSt(x: Float) = if x % 1 == 0 then f"$x%.0f" else f"$x%.1f"

def getFileName(title: String, addTimeStamp: Boolean, name: String, default: String) =
  (title, name) match
    case ("", "") => default
    case ("", _)  => name
    case _        => title.replaceAll(" ", "_")
    + (if addTimeStamp then "_" + timeSt() else "") + ".html"

def isRegular[A](l: Seq[Seq[A]]): Boolean =
  l.length < 2 || l.forall(a => a.length == l.head.length)

def stringAsIntArr(s: String): Array[Array[Int]] =
  s.trim.split("\n").map(_.trim.map(_.toInt - 48).toArray)

def stringAsIntSeq(s: String): Seq[Seq[Int]] =
  s.trim.split("\n").map(_.trim.map(_.toInt - 48).toList).toList

def streamToLazyList[A](st: BaseStream[A, ?]): LazyList[A] =
  LazyList.from(st.iterator.asScala)

def streamToList[A](st: BaseStream[A, ?]): JList[A] =
  StreamSupport.stream(st.spliterator(), false).toList

object StringExtensions:
  extension [A](s: String)
    infix def stripMrgn =
      val firstMargin = (if s(0) == '\n' then s.drop(1) else s).indexWhere(_ != ' ')
      s.split("\n").map(x => if x.indexWhere(_ != ' ') >= firstMargin then x.drop(firstMargin) else x).mkString("\n")

trait ChannelT[A]:
  def delayedRead(delay: Int): A
  def delayedSend(a: A, delay: Int): Unit
  def clear(): Unit

case class Channel[A](name: String, queue: ConcurrentLinkedDeque[A]) extends ChannelT[A]:

  def defaultDelay = 25
  def read(): A = delayedRead(0)
  def delayedRead(delay: Int = defaultDelay): A =
    Thread.sleep(delay)
    queue.getLast()
  def send(a: A): Unit = delayedSend(a, 0)
  def delayedSend(a: A, delay: Int = defaultDelay): Unit =
    Thread.sleep(delay)
    queue.add(a)
  def contents =
    queue.toArray.toList.asInstanceOf[List[A]]
  def clear(): Unit =
    queue.clear()

object Channel:
  def apply[A](name: String): Channel[A] =
    Channel(name, ConcurrentLinkedDeque[A]())

object JavaUtil:

  def complement[T](l: JList[T], full: JList[T]): JList[T] =
    full.asScala.filter(!l.contains(_)).asJava
  def complement[T](l: JList[T], full: BaseStream[T, ?]): JList[T] =
    streamToLazyList(full).filter(!l.contains(_)).asJava

  def complement[T](st: BaseStream[JInteger, ?], full: BaseStream[JInteger, ?]): IntStream =
    val l1 = st.iterator().asScala.toList
    val l2 = streamToLazyList(full).filter(!l1.contains(_))
    StreamSupport.stream(Spliterators.spliteratorUnknownSize(l2.iterator.asJava, 0), false).mapToInt(x => x)

  def named[A](a: A, name: String) = Named(a, name)
  def namedFn[A, B](f: JFunction[A, B], name: String) = Named(f, name)

  case class Pair[A, B](first: A, second: B):
    override def toString(): String = s"[$first $second]"
  case class Triple[A, B, C](first: A, second: B, third: C):
    override def toString(): String = s"[$first $second $third]"

  def zip[A, B](l1: JList[A], l2: JList[B]): JList[Pair[A, B]] =
    (l1.asScala zip l2.asScala).map(Pair(_, _)).asJava

  def zip[A, B](st: BaseStream[A, ?], l: JList[B]): JList[Pair[A, B]] =
    (streamToLazyList(st) zip l.asScala).map(Pair(_, _)).asJava

  def toMap[A, B](as: JList[A], bs: JList[B]): JMap[A, B] =
    (as.asScala zip bs.asScala).toMap.asJava

  def map[A, B](l: JList[A], f: JFunction[A, B]): JList[B] =
    l.asScala.map(f(_)).asJava

  def filter[A](l: JList[A], f: JFunction[A, Boolean]): JList[A] =
    l.asScala.filter(f(_)).asJava

  def append[A](l1: Seq[A], l2: Seq[A]) = l1 ++ l2
  def append[A](l1: JList[A], l2: JList[A]): JList[A] =
    java.util.stream.Stream.concat(l1.stream, l2.stream).toList

  def stringAsIntArr(s: String): Array[Array[Int]] =
    s.trim.split("\n").map(_.trim.map(_.toInt - 48).toArray)

  def stringAsIntSeq(s: String): JList[JList[JInteger]] =
    s.trim.split("\n").map(_.trim.map(x => (java.lang.Integer)(x.toInt - 48)).toList.asJava).toList.asJava

  def toMap[A, B](l: JList[Pair[A, B]]): JMap[A, B] =
    l.asScala.map(p => (p.first, p.second)).toMap.asJava

  def drop[A](l: JList[A], n: Int) =
    l.asScala.drop(n).asJava

  def subArray[X, A, B](a: MultiArrayB[X, A, B], i: Int, j: Int, la: Int, lb: Int): JList[JList[X]] =
    a.subArray(i, j, la, lb).map(_.asJava).asJava

  def getClassTag[A](a: A) =
    ClassTag(a.getClass).asInstanceOf[ClassTag[A]]

  def some[A](a: A): Option[A] = Some(a)
  def somePair[A, B](a: A, b: B) = Some(a, b)
  def none[A]() = Option.empty[A]
