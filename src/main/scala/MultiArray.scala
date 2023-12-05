package multiarray

import augmented._
import augmented.given
import basicdef._
import shape._
import util._
import util.JavaUtil.Pair

import collection.JavaConverters._
import math.Numeric.Implicits.infixNumericOps
import scala.reflect.ClassTag
import java.util.stream.BaseStream

case class MultiArrayA[X, A](as: Seq[A], arr: Array[X])(using ClassTag[X]) extends MultiArrayT[X]:

  val length = as.length
  val axisLengths = List(1)
  def flat() = arr.toList
  def isEmpty = as.isEmpty
  def axes = List(as)

  def apply(a: A) =
    val ia = as.indexOf(a)
    val i = getIndex(List(ia), axisLengths, length, List(a))
    arr(i)

  def transform[Z](f: X => Z)(using ClassTag[Z]) =
    MultiArrayA(as, arr.map(f))

  def transform[Z](f: (X, A) => Z)(using ClassTag[Z]) =
    multiArray(as, (arr zip as).map(f(_, _)))

  def toSeq(): Seq[(A, X)] = (as zip arr)
  def toSeq[Z](f: X => Z)(using ClassTag[Z]): Seq[(A, Z)] = (as zip arr.map(f))

  def plot(title: String, addTimeStamp: Boolean, name: String, visibleAxes: Boolean) = ???

  override def toString(): String = s"[A:$length]\n${arr.mkString(" ")}\n"

trait MultiArrayT[X]:
  val length: Int
  def flat(): Seq[X]
  def isEmpty: Boolean
  def plot(title: String, addTimeStamp: Boolean, name: String, visibleAxes: Boolean): Unit
  def vec() = clojure.lang.PersistentVector.create(flat().asJava)

object multiArray:

  def apply[X, A](as: Seq[A], na: Seq[X])(using ClassTag[X]) =
    checkedMultiArray(as, na.toArray)

  def apply[X, A, B](as: Seq[A], bs: Seq[B], na: Array[Array[X]])(using ClassTag[X]) =
    checkedMultiArray(as, bs, na.flatten)

  def apply[X, A, B](as: Seq[A], bs: Seq[B], na: Seq[Seq[X]])(using ClassTag[X]) =
    checkedMultiArray(as, bs, na.flatten.toArray)

  def apply[X, A, B](as: Seq[A], bs: Seq[B], arr: Array[X])(using ClassTag[X]) =
    checkedMultiArray(as, bs, arr)

  def apply[X](na: Array[Array[X]])(using ClassTag[X]) =
    val l = na.map(_.toList).toList
    if l.length > 0 && isRegular(l) then checkedMultiArray(0 until na.length, 0 until na.head.length, na.flatten)
    else throw new Exception(s"Non-rectangular inputs used to construct array")

  def apply[X, A, B, C](as: Seq[A], bs: Seq[B], cs: Seq[C], na: Seq[Seq[Seq[X]]])(using ClassTag[X]) =
    checkedMultiArray(as, bs, cs, na.flatten.flatten.toArray)

  def apply[X, A, B, C](as: Seq[A], bs: Seq[B], cs: Seq[C], arr: Array[X])(using ClassTag[X]) =
    checkedMultiArray(as, bs, cs, arr)

  def apply[X, A, B, C, D](as: Seq[A], bs: Seq[B], cs: Seq[C], ds: Seq[D], na: Seq[Seq[Seq[Seq[X]]]])(using
      ClassTag[X]
  ) =
    checkedMultiArray(as, bs, cs, ds, na.flatten.flatten.flatten.toArray)

  def apply[X, A, B, C, D](as: Seq[A], bs: Seq[B], cs: Seq[C], ds: Seq[D], arr: Array[X])(using ClassTag[X]) =
    checkedMultiArray(as, bs, cs, ds, arr)

  def apply[X, A, B, C, D, E](
      as: Seq[A],
      bs: Seq[B],
      cs: Seq[C],
      ds: Seq[D],
      es: Seq[E],
      na: Seq[Seq[Seq[Seq[Seq[X]]]]]
  )(using ClassTag[X]) =
    checkedMultiArray(as, bs, cs, ds, es, na.flatten.flatten.flatten.flatten.toArray)

  def apply[X, A, B, C, D, E](as: Seq[A], bs: Seq[B], cs: Seq[C], ds: Seq[D], es: Seq[E], arr: Array[X])(using
      ClassTag[X]
  ) =
    checkedMultiArray(as, bs, cs, ds, es, arr)

  def apply[X, A, B, C, D](as: Seq[A], bs: Seq[B], blocks: MultiArrayB[MultiArrayB[X, C, D], A, B])(using ClassTag[X]) =
    val arr = image(as, bs, blocks(_, _).arr).arr.flatten
    val (cs, ds) = if !blocks.isEmpty then (blocks.head.as, blocks.head.bs) else (List(), List())
    checkedMultiArray(as, bs, cs, ds, arr)

  def multiarray[X, A, B](as: JList[A], bs: JList[B], na: JList[JList[X]]) =
    val l1 = as.asScala.toList
    val l2 = bs.asScala.toList
    val ln = na.asScala.toList.map(_.asScala.toList)
    given ClassTag[X] = ClassTag(ln.head.head.getClass)
    checkedMultiArray(l1, l2, ln.flatten.toArray)

  def multiarray[X, A, B, C](as: JList[A], bs: JList[B], cs: JList[C], na: JList[JList[JList[X]]]) =
    val l1 = as.asScala.toList
    val l2 = bs.asScala.toList
    val l3 = cs.asScala.toList
    val ln = na.asScala.toList.map(_.asScala.toList.map(_.asScala.toList))
    given ClassTag[X] = ClassTag(ln.head.head.head.getClass)
    checkedMultiArray(l1, l2, l3, ln.flatten.flatten.toArray)

  def multiarray[X, A, B, C](as: JList[A], bs: JList[B], cs: JList[C], na: Seq[Seq[Seq[X]]]) =
    val l1 = as.asScala.toList
    val l2 = bs.asScala.toList
    val l3 = cs.asScala.toList
    given ClassTag[X] = ClassTag(na.head.head.head.getClass)
    checkedMultiArray(l1, l2, l3, na.flatten.flatten.toArray)

  def apply[X](na: Array[Array[Array[X]]])(using ClassTag[X]) =
    val l = na.map(_.map(_.toList).toList).toList
    if l.length > 0 && l.head.length > 0 && isRegular(l) then
      checkedMultiArray(
        0 until na.length,
        0 until na.head.length,
        0 until na.head.head.length,
        l.flatten.flatten.toArray
      )
    else throw new Exception(s"Non-rectangular inputs used to construct array")

  def multiarray[X, A, B](as: BaseStream[A, _], bs: BaseStream[B, _], na: JList[JList[X]]): MultiArrayB[X, A, B] =
    multiarray(streamToList(as), streamToList(bs), na)

  def multiarray[X, A, B](as: BaseStream[A, _], bs: BaseStream[B, _], na: JStream[JStream[X]]): MultiArrayB[X, A, B] =
    val l: JList[JList[X]] = streamToList(na).asScala.map(streamToList).asJava
    multiarray(as, bs, l)

  def multiarray[X, A, B, C](
      as: BaseStream[A, _],
      bs: BaseStream[B, _],
      cs: BaseStream[C, _],
      na: JList[JList[JList[X]]]
  ): MultiArrayC[X, A, B, C] =
    multiarray(streamToList(as), streamToList(bs), streamToList(cs), na)

  def multiarray[X, A, B, C, D <: BaseStream[X, _]](
      as: BaseStream[A, _],
      bs: BaseStream[B, _],
      cs: BaseStream[C, _],
      na: JStream[JStream[D]]
  ): MultiArrayC[X, A, B, C] =
    val l = na.map(_.map(streamToList(_)).toList).toList
    multiarray(streamToList(as), streamToList(bs), streamToList(cs), l)

  def plus[Z, A, B](na: MultiArrayB[Z, A, B], nb: MultiArrayB[Z, A, B])(using
      Numeric[Z],
      ClassTag[Z]
  ): MultiArrayB[Z, A, B] =
    if na.axisLengths != nb.axisLengths then
      throw new Exception(s"Arrays must have same dimensions: ${na.axisLengths} vs ${nb.axisLengths}")
    val ys = (na.arr zip nb.arr).map(_ + _)
    multiArray(na.as, na.bs, ys)

trait ArrayB[X](val arr: Array[X]):
  def length: Int = 0

def getIndex(subIndexes: Seq[Int], axisLengths: Seq[Int], l: Int, coords: Seq[Any]) =
  if (subIndexes.contains(-1)) then throw new Exception(s"Element not found: ${coords.mkString(".")}")
  (subIndexes zip axisLengths).map(_ * _).sum

def getIndexOpt(subIndexes: Seq[Int], axisLengths: Seq[Int], l: Int, coords: Seq[Any]) =
  if (subIndexes.contains(-1)) then None
  else Some((subIndexes zip axisLengths).map(_ * _).sum)

val entryDividingLine = "_" * 30

def checkInputDim[X, A](as: Seq[A], xs: Seq[X]) =
  if as.length != xs.length then throw new Exception(s"Non-rectangular input lengths: ${as.length} ${xs.length}")
def checkInputDim[X, A, B](as: Seq[A], bs: Seq[B], xs: Seq[X]) =
  if as.length * bs.length != xs.length then
    throw new Exception(s"Non-rectangular input lengths: ${as.length} ${bs.length} ${xs.length}")
def checkInputDim[X, A, B, C](as: Seq[A], bs: Seq[B], cs: Seq[C], xs: Seq[X]) =
  if as.length * bs.length * cs.length != xs.length then
    throw new Exception(s"Non-rectangular input lengths: ${as.length} ${bs.length} ${cs.length} ${xs.length}")
def checkInputDim[X, A, B, C, D](as: Seq[A], bs: Seq[B], cs: Seq[C], ds: Seq[D], xs: Seq[X]) =
  if as.length * bs.length * cs.length * ds.length != xs.length then
    throw new Exception(
      s"Non-rectangular input lengths: ${as.length} ${bs.length} ${cs.length} ${ds.length} ${xs.length}"
    )
def checkInputDim[X, A, B, C, D, E](as: Seq[A], bs: Seq[B], cs: Seq[C], ds: Seq[D], es: Seq[E], xs: Seq[X]) =
  if as.length * bs.length * cs.length * ds.length * es.length != xs.length then
    throw new Exception(
      s"Non-rectangular input lengths: ${as.length} ${bs.length} ${cs.length} ${ds.length} ${es.length} ${xs.length}"
    )

def checkedMultiArray[X, A](as: Seq[A], xs: Array[X])(using ClassTag[X]) =
  checkInputDim(as, xs)
  MultiArrayA(as, xs)
def checkedMultiArray[X, A, B](as: Seq[A], bs: Seq[B], xs: Array[X])(using ClassTag[X]) =
  checkInputDim(as, bs, xs)
  MultiArrayB(as, bs, xs)
def checkedMultiArray[X, A, B, C](as: Seq[A], bs: Seq[B], cs: Seq[C], xs: Array[X])(using ClassTag[X]) =
  checkInputDim(as, bs, cs, xs)
  MultiArrayC(as, bs, cs, xs)
def checkedMultiArray[X, A, B, C, D](as: Seq[A], bs: Seq[B], cs: Seq[C], ds: Seq[D], xs: Array[X])(using ClassTag[X]) =
  checkInputDim(as, bs, cs, ds, xs)
  MultiArrayD(as, bs, cs, ds, xs)
def checkedMultiArray[X, A, B, C, D, E](as: Seq[A], bs: Seq[B], cs: Seq[C], ds: Seq[D], es: Seq[E], xs: Array[X])(using
    ClassTag[X]
) =
  checkInputDim(as, bs, cs, ds, es, xs)
  MultiArrayE(as, bs, cs, ds, es, xs)

case class MultiArrayB[X, A, B](as: Seq[A], bs: Seq[B], override val arr: Array[X])(using ClassTag[X])
    extends ArrayB(arr)
    with MultiArrayT[X]:

  def this(as: Seq[A], bs: Seq[B], na: Seq[Seq[X]])(using ClassTag[X]) =
    this(as, bs, na.flatten.toArray)

  override val length = as.length * bs.length
  def flat(): Seq[X] = arr.toList
  def nested(): Seq[Seq[X]] = augment.seq(this(_, _))(as, bs)
  def nestedAsJava(): JList[JList[X]] = nested().map(_.asJava).asJava
  def isEmpty = as.isEmpty || bs.isEmpty
  def axes = List(as, bs)

  def apply(a: A, b: B) =
    val ia = as.indexOf(a)
    val ib = bs.indexOf(b)
    val i = getIndex(List(ia, ib), axisLengths, length, List(a, b))
    arr(i)

  def apply(coords: (A, B)): X = apply(coords._1, coords._2)

  def get(a: A, b: B): Option[X] =
    val ia = as.indexOf(a)
    val ib = bs.indexOf(b)
    if ia >= 0 && ib >= 0 then
      val i = getIndex(List(ia, ib), axisLengths, length, List(a, b))
      if i >= 0 && i < arr.length then Some(arr(i))
      else None
    else None

  def get(a: A, b: B, default: X): X =
    get(a, b) match
      case Some(x) => x
      case None    => default

  def getByIndex(ia: Int, ib: Int): Option[X] =
    if ia >= 0 && ib >= 0 then
      val i = getIndex(List(ia, ib), axisLengths, length, List())
      if i >= 0 && i < arr.length then Some(arr(i))
      else None
    else None

  def la = as.length
  def lb = bs.length

  def rows() =
    (0 until as.length).map(i => (0 until bs.length).map(j => arr(i * bs.length + j))).toVector.toVector

  def cols() =
    (0 until bs.length).map(i => (0 until as.length).map(j => arr(j * bs.length + i))).toVector.toVector

  def diags() =
    if as.length != bs.length then None
    else
      val l = as.length
      Some((as zip bs).map(this(_, _)), (as zip bs.reverse).map(this(_, _)))

  def row(a: A) = rows()(as.indexOf(a))
  def col(b: B) = cols()(bs.indexOf(b))

  def head = apply(as.head, bs.head)

  def subArray(a: A): MultiArrayA[X, B] =
    val ia = as.indexOf(a)
    val subSeq = nested()(ia)
    multiArray(bs, subSeq)

  def subArray[Z](a: A, f: X => Z)(using ClassTag[Z]): MultiArrayA[Z, B] =
    val ia = as.indexOf(a)
    val subSeq = nested()(ia).map(f)
    multiArray(bs, subSeq)

  def subArray(i: Int, j: Int, la: Int, lb: Int): Seq[Seq[X]] =
    nested().slice(i * la, i * la + lb).map(_.slice(j * lb, j * lb + la))

  def plus(na: MultiArrayB[X, A, B])(using Numeric[X]): MultiArrayB[X, A, B] =
    if na.axisLengths != this.axisLengths then
      throw new Exception(s"Arrays must have same dimensions: ${na.axisLengths} vs ${this.axisLengths}")
    val ys = (this.arr zip na.arr).map(_ + _)
    multiArray(as, bs, ys)

  def plus(na: ArrayB[JInteger]): MultiArrayB[X, A, B] =
    given Numeric[X] = Numeric[Int].asInstanceOf[Numeric[X]]
    plus(na.asInstanceOf[ArrayB[X]])

  def plus(na: ArrayB[X])(using Numeric[X]): MultiArrayB[X, A, B] =
    val ys = (this.arr zip na.arr).map(_ + _)
    multiArray(as, bs, ys)

  def appendTo(na: MultiArrayB[List[X], A, B])(using Numeric[X]): MultiArrayB[List[X], A, B] =
    if na.axisLengths != this.axisLengths then
      throw new Exception(s"Arrays must have same dimensions: ${na.axisLengths} vs ${this.axisLengths}")
    val ys = (na.arr zip this.arr).map(_ :+ _)
    multiArray(as, bs, ys)

  def select(phi: X => Boolean): Seq[(A, B)] =
    tupleB(as, bs, (a: A, b: B) => phi(this(a, b)))

  def selectPairs(phi: X => Boolean): JList[Pair[A, B]] =
    var f = augment((x: A, y: B) => Pair(x, y))
    f(as.asJava, (a: A) => bs.asJava, (a: A, b: B) => phi(this(a, b)))

  def transform[Z](f: X => Z)(using ClassTag[Z]) =
    multiArray(as, bs, nested().map(_.map(f)))

  def transform[Z](f: (X, A, B) => Z)(using ClassTag[Z]) =
    multiArray(as, bs, (nested() zip as).map((x, a) => (x zip bs).map((x, b) => f(x, a, b))))

  def transformIfMap[C, Y](cs: Seq[C])(using ClassTag[C], ClassTag[Y]) =
    multiArray(
      as,
      bs,
      cs,
      nested().map(_.map(x =>
        if !x.isInstanceOf[Map[_, _]] then
          throw new Exception(s"Expected values of type map but found: ${x.getClass}:\n$x\n")
        val m = x.asInstanceOf[Map[C, Y]]
        cs.map(c => m(c))
      ))
    )

  def transformIfMap[C, Y, Z](cs: Seq[C], altC: C => Z)(using ClassTag[C], ClassTag[Y], ClassTag[Y | Z]) =
    multiArray(
      as,
      bs,
      cs,
      nested().map(_.map(x =>
        if !x.isInstanceOf[Map[_, _]] then
          throw new Exception(s"Expected values of type map but found: ${x.getClass}:\n$x\n")
        val m = x.asInstanceOf[Map[C, Y]]
        cs.map(c => m.getOrElse(c, altC(c)))
      ))
    )

  def transpose(): MultiArrayB[X, B, A] =
    image(bs, as, (b, a) => this(a, b))

  val axisLengths = List(bs.length, 1)

  def plot(title: String = "", addTimeStamp: Boolean = false, name: String = "", visibleAxes: Boolean = true) =
    multiarrayplot.PlotExtensionsB.plot(this)(title, addTimeStamp, name, visibleAxes)

  def plotFlat(title: String = "", addTimeStamp: Boolean = false, name: String = "", visibleAxes: Boolean = true) =
    multiarrayplot.PlotExtensionsB.plotFlat(this)(title, addTimeStamp, name, visibleAxes)

  def plotContour(title: String = "", addTimeStamp: Boolean = false, name: String = "", visibleAxes: Boolean = true) =
    multiarrayplot.PlotExtensionsB.plotContour(this)(title, addTimeStamp, name, visibleAxes)

  def plotHeatMap(title: String = "", addTimeStamp: Boolean = false, name: String = "", visibleAxes: Boolean = true) =
    multiarrayplot.PlotExtensionsB.plotHeatMap(this)(title, addTimeStamp, name, visibleAxes)

  def graph() = multiarrayplot.PlotExtensionsB.graph(this)()

  def saveToFile(
      folder: String,
      fileNm: String,
      hasMultiLineEntry: Boolean = false,
      entrySeparator: String = entryDividingLine,
      entryToString: X => String = _.toString
  ) =
    val s1 = s"${as.mkString("|")}\n${bs.mkString("|")}\n$entryDividingLine\n"
    val separator = if hasMultiLineEntry then entrySeparator + "\n" else ""
    val s2 = arr.map(entryToString).mkString(s"\n$separator")
    util.saveToFile(folder, fileNm, s"$s1$s2\n", "array")

  override def toString(): String =
    toString(_.toString)

  def toString(replaceFn: X => String = _.toString) =
    val disp =
      nested().map:
        _.map: x =>
          x match
            case b: Boolean => if b then 1 else 0
            case _          => replaceFn(x)
    "\n" + disp.reverse.map(_.mkString(" ")).mkString("\n") + "\n"

// A, B, C <-> Z, Y, X

case class MultiArrayC[X, A, B, C](as: Seq[A], bs: Seq[B], cs: Seq[C], arr: Array[X])(using
    ClassTag[X]
) extends MultiArrayT[X]:

  val length = as.length * bs.length * cs.length
  def lengths(i: Int) = List(as, bs, cs)(i).length
  def flat(): Seq[X] = arr.toList
  def nested(): Seq[Seq[Seq[X]]] = augment.seq(this(_, _, _))(as, bs, cs)
  def nestedAsJava(): JList[JList[JList[X]]] = nested().map(_.map(_.asJava).asJava).asJava
  def isEmpty = as.isEmpty || bs.isEmpty || cs.isEmpty
  def axes = List(as, bs, cs)
  val axisLengths = List(bs.length * cs.length, cs.length, 1)
  def head = apply(as.head, bs.head, cs.head)

  def apply(a: A, b: B, c: C): X =
    val ia = as.indexOf(a)
    val ib = bs.indexOf(b)
    val ic = cs.indexOf(c)
    val i = getIndex(List(ia, ib, ic), axisLengths, length, List(a, b, c))
    arr(i)

  def get(a: A, b: B, c: C): Option[X] =
    val ia = as.indexOf(a)
    val ib = bs.indexOf(b)
    val ic = cs.indexOf(c)
    val iOpt = getIndexOpt(List(ia, ib, ic), axisLengths, length, List(a, b, c))
    iOpt.map(arr(_))

  def subArray(a: A): MultiArrayB[X, B, C] =
    val ia = as.indexOf(a)
    val subSeq = nested()(ia)
    multiArray(bs, cs, subSeq)

  def subArray[Z](a: A, f: X => Z)(using ClassTag[Z]): MultiArrayB[Z, B, C] =
    val ia = as.indexOf(a)
    val subSeq = nested()(ia).map(_.map(f))
    multiArray(bs, cs, subSeq)

  def toSubArrays: List[(A, MultiArrayB[X, B, C])] =
    as.map(a => (a, subArray(a))).toList

  def plus(na: MultiArrayC[X, A, B, C])(using Numeric[X]): MultiArrayC[X, A, B, C] =
    if na.axisLengths != this.axisLengths then
      throw new Exception(s"Arrays must have same dimensions: ${na.axisLengths} vs ${this.axisLengths}")
    val ys = (this.arr zip na.arr).map(_ + _)
    multiArray(as, bs, cs, ys)

  def sumAlongA[Z](f: X => Z)(using Numeric[Z], ClassTag[Z]): MultiArrayB[Z, B, C] =
    val subArrays = as.map(subArray(_, f))
    val zArr = subArrays.head.transform(_ => Numeric[Z].zero)
    subArrays.foldLeft(zArr)(_.plus(_))

  def seqAlongA(): MultiArrayB[List[X], B, C] =
    val subArrays = as.map(subArray(_, x => x))
    val zArr = subArrays.head.transform(_ => List[X]())
    val ls =
      subArrays.map(_.arr.toList).drop(1).foldLeft(subArrays.head.arr.map(List(_)))((x, y) => (x zip y).map(_ :+ _))
    MultiArrayB(bs, cs, ls.toArray)

  def arrayOfSeqs(): MultiArrayB[Seq[X], A, B] =
    val subSeq = nested()
    multiArray[Seq[X], A, B](as, bs, subSeq)

  def select(phi: X => Boolean): Seq[(A, B, C)] =
    tupleC(as, bs, cs, (a: A, b: B, c: C) => phi(this(a, b, c)))

  def transform[Z](f: X => Z)(using ClassTag[Z]) =
    multiArray(as, bs, cs, arr.map(f))

  def transform[Z](f: (X, A, B, C) => Z)(using ClassTag[Z]) =
    MultiArrayC(
      as,
      bs,
      cs,
      (nested() zip as)
        .map((l1, a) => (l1 zip bs).map((l2, b) => (l2 zip cs).map((x, c) => f(x, a, b, c))))
        .flatten
        .flatten
        .toArray
    )

  def plot(title: String = "", addTimeStamp: Boolean = false, name: String = "", visibleAxes: Boolean = true) =
    multiarrayplot.PlotExtensionsC.plot(this)(title, addTimeStamp, name, visibleAxes)

  def graph() = multiarrayplot.PlotExtensionsC.graph(this)()

  def animate(
      title: String = "",
      flat: Boolean = false,
      duration: Duration = 750,
      addTimeStamp: Boolean = false,
      name: String = "",
      visibleAxes: Boolean = true,
      imageSources: Option[(URL, URL)] = None,
      getGridColors: (Int, Int) => Color = (_, _) => Color("#FFFFFF")
  ) =
    multiarrayplot.PlotExtensionsC
      .animate(this)(title, flat, duration, addTimeStamp, name, visibleAxes, imageSources, getGridColors)

  def animate(
      title: String,
      flat: Boolean,
      duration: Duration,
      visibleAxes: Boolean,
      imageSrcs: Option[(URL, URL)] | Boolean,
      getGridColors: (Integer, Integer) => String
  ) =
    var imageSources = imageSrcs match
      case b: Boolean              => None
      case opt: Option[(URL, URL)] => opt
    multiarrayplot.PlotExtensionsC
      .animate(this)(title, flat, duration, false, "", visibleAxes, imageSources, (a, b) => Color(getGridColors(a, b)))

  override def toString(): String =
    val disp =
      nested().map:
        _.map:
          _.map: x =>
            x match
              case b: Boolean => if b then 1 else 0
              case _          => x
    "\n" + disp.map(_.reverse.map(_.mkString(" ")).mkString("\n") + "\n").mkString("\n")

case class MultiArrayD[X, A, B, C, D](as: Seq[A], bs: Seq[B], cs: Seq[C], ds: Seq[D], arr: Array[X])(using ClassTag[X])
    extends MultiArrayT[X]:

  val length = as.length * bs.length * cs.length * ds.length
  def flat(): Seq[X] = arr.toList
  def nested(): Seq[Seq[Seq[Seq[X]]]] = augment.seq(this(_, _, _, _))(as, bs, cs, ds)
  def nestedAsJava(): JList[JList[JList[JList[X]]]] = nested().map(_.map(_.map(_.asJava).asJava).asJava).asJava
  def isEmpty = as.isEmpty || bs.isEmpty || cs.isEmpty || ds.isEmpty
  def axes = List(as, bs, cs, ds)

  def apply(a: A, b: B, c: C, d: D) =
    val ia = as.indexOf(a)
    val ib = bs.indexOf(b)
    val ic = cs.indexOf(c)
    val id = ds.indexOf(d)
    val i = getIndex(List(ia, ib, ic, id), axisLengths, length, List(a, b, c, d))
    arr(i)

  def map[Z](f: Seq[X] => Z)(using ClassTag[Z]) =
    val sq =
      as.map: a =>
        bs.map: b =>
          f(
            cs.map: c =>
              ds.map: d =>
                this(a, b, c, d)
            .flatten
          )
    multiArray(as, bs, sq)

  def map[Z](f: JList[X] => Z) =
    given ClassTag[Z] = ClassTag(this.head.getClass)
    val sq =
      as.map: a =>
        bs.map: b =>
          f(
            cs.map: c =>
              ds.map: d =>
                this(a, b, c, d)
            .flatten
              .asJava
          )
    multiArray(as, bs, sq)

  def head = apply(as.head, bs.head, cs.head, ds.head)

  def subArray[Z](a: A, f: X => Z)(using ClassTag[Z]): MultiArrayC[Z, B, C, D] =
    val ia = as.indexOf(a)
    if ia == -1 then throw new Exception(s"Index $a not found in ${as}\n${this}")
    val subIndexes = Array(ia, 0, 0, 0, 0)
    val ind = getIndex(subIndexes, axisLengths, length, List(a))
    val arr1 = arr.slice(ind, ind + axisLengths(0))
    multiArray(bs, cs, ds, arr1.map(f))

  def subArray[Z](a: A, b: B, f: X => Z)(using ClassTag[Z]): MultiArrayB[Z, C, D] =
    val ia = as.indexOf(a)
    val ib = bs.indexOf(b)
    if ia == -1 then throw new Exception(s"Index $a not found in ${as}\n${this}")
    if ib == -1 then throw new Exception(s"Index $b not found in ${bs}\n${this}")
    val subIndexes = Array(ia, ib, 0, 0, 0)
    val ind = getIndex(subIndexes, axisLengths, length, List(a, b))
    val arr1 = arr.slice(ind, ind + axisLengths(1))
    multiArray(cs, ds, arr1.map(f))

  def blockArrayA(): MultiArrayA[MultiArrayC[X, B, C, D], A] =
    MultiArrayA(as, as.map(subArray(_, x => x)).toArray)

  def blockArrayB(): MultiArrayB[MultiArrayB[X, C, D], A, B] =
    val arr1 = nested().map(_.map(multiArray(cs, ds, _)))
    multiArray(as, bs, arr1)

  def sumAlongA[Z](f: X => Z)(using Numeric[Z], ClassTag[Z]): MultiArrayC[Z, B, C, D] =
    val subArrays = as.map(subArray(_, f))
    val zArr = subArrays.head.transform(_ => Numeric[Z].zero)
    subArrays.foldLeft(zArr)(_.plus(_))

  val axisLengths = List(bs.length * cs.length * ds.length, cs.length * ds.length, ds.length, 1)

  def transform[Z](f: X => Z)(using ClassTag[Z]) =
    MultiArrayD(as, bs, cs, ds, arr.map(f))

  def transform[Z](f: (X, A, B, C, D) => Z)(using ClassTag[Z]) =
    MultiArrayD(
      as,
      bs,
      cs,
      ds,
      (nested() zip as)
        .map((l1, a) =>
          (l1 zip bs).map((l2, b) => (l2 zip cs).map((l3, c) => (l3 zip ds).map((x, d) => f(x, a, b, c, d))))
        )
        .flatten
        .flatten
        .flatten
        .toArray
    )

  def saveToFiles(
      folder: String,
      filePrefix: String,
      fileSuffix: String,
      hasMultiLineEntry: Boolean = false,
      entrySeparator: String = entryDividingLine,
      entryToString: X => String = _.toString
  ) =
    val s1 = s"${axes.map(_.mkString("|")).mkString("\n")}\n$entryDividingLine\n"
    val separator = if hasMultiLineEntry then entrySeparator + "\n" else ""
    val s2 = arr.map(entryToString).mkString(s"\n$separator")
    util.saveToFile(folder, filePrefix + fileSuffix, s"$s1$s2\n", "array")

    def saveToFile(a: A, b: B) =
      val arr1 = subArray(a, b, x => x)
      val fileNm = s"${filePrefix}_${a}_$b.txt"
      arr1.saveToFile(folder, fileNm, hasMultiLineEntry, entrySeparator, entryToString)
    augment(saveToFile)(as, bs)

  def plot(title: String, addTimeStamp: Boolean, name: String, visibleAxes: Boolean) = ???

  override def toString(): String =
    "\n" + (nested() zip as)
      .map((x, a) =>
        s"$a\n" + (x zip bs)
          .map((y, b) =>
            s"  $b\n" +
              (y zip cs)
                .map((z, c) =>
                  s"$c\t" + ds.mkString(" ") +
                    (if ds.length == 1 then " | " else "\n\t") + z.mkString("\n\t")
                )
                .mkString("\n") + "\n"
          )
          .mkString("\n")
      )
      .mkString("\n")

case class MultiArrayE[X, A, B, C, D, E](
    as: Seq[A],
    bs: Seq[B],
    cs: Seq[C],
    ds: Seq[D],
    es: Seq[E],
    arr: Array[X]
)(using ClassTag[X])
    extends MultiArrayT[X]:

  val length = as.length * bs.length * cs.length * ds.length * es.length
  def flat(): Seq[X] = arr.toList
  def nested(): Seq[Seq[Seq[Seq[Seq[X]]]]] = augment.seq(this(_, _, _, _, _))(as, bs, cs, ds, es)
  def nestedAsJava(): JList[JList[JList[JList[JList[X]]]]] =
    nested().map(_.map(_.map(_.map(_.asJava).asJava).asJava).asJava).asJava
  def isEmpty = as.isEmpty || bs.isEmpty || cs.isEmpty || ds.isEmpty || es.isEmpty
  def axes = List(as, bs, cs, ds, es)

  def apply(a: A, b: B, c: C, d: D, e: E) =
    val ia = as.indexOf(a)
    val ib = bs.indexOf(b)
    val ic = cs.indexOf(c)
    val id = ds.indexOf(d)
    val ie = es.indexOf(e)
    val i = getIndex(Array(ia, ib, ic, id, ie), axisLengths, length, List(a, b, c, d, e))
    arr(i)

  val axisLengths = List(
    bs.length * cs.length * ds.length * es.length,
    cs.length * ds.length * es.length,
    ds.length * es.length,
    es.length,
    1
  )

  def head = apply(as.head, bs.head, cs.head, ds.head, es.head)

  def subArray[Z](a: A, f: X => Z)(using ClassTag[Z]): MultiArrayD[Z, B, C, D, E] =
    val ia = as.indexOf(a)
    if ia == -1 then throw new Exception(s"Index $a not found in ${as}\n${this}")
    val subIndexes = Array(ia, 0, 0, 0, 0)
    val ind = getIndex(subIndexes, axisLengths, length, List(a))
    val arr1 = arr.slice(ind, ind + axisLengths(2))
    multiArray(bs, cs, ds, es, arr1.map(f))

  def subArray[Z](a: A, b: B, f: X => Z)(using ClassTag[Z]): MultiArrayC[Z, C, D, E] =
    val ia = as.indexOf(a)
    val ib = bs.indexOf(b)
    if ia == -1 then throw new Exception(s"Index $a not found in ${as}\n${this}")
    if ib == -1 then throw new Exception(s"Index $b not found in ${bs}\n${this}")
    val subIndexes = Array(ia, ib, 0, 0, 0)
    val ind = getIndex(subIndexes, axisLengths, length, List(a, b))
    val arr1 = arr.slice(ind, ind + axisLengths(2))
    multiArray(cs, ds, es, arr1.map(f))

  def subArray[Z](a: A, b: B, c: C, f: X => Z)(using ClassTag[Z]): MultiArrayB[Z, D, E] =
    val ia = as.indexOf(a)
    val ib = bs.indexOf(b)
    val ic = cs.indexOf(c)
    if ia == -1 then throw new Exception(s"Index $a not found in ${as}\n${this}")
    if ib == -1 then throw new Exception(s"Index $b not found in ${bs}\n${this}")
    if ic == -1 then throw new Exception(s"Index $c not found in ${cs}\n${this}")
    val subIndexes = Array(ia, ib, ic, 0, 0)
    val ind = getIndex(subIndexes, axisLengths, length, List(a, b, c))
    val arr1 = arr.slice(ind, ind + axisLengths(2))
    MultiArrayB(ds, es, arr1.map(f))

  def plot(title: String, addTimeStamp: Boolean, name: String, visibleAxes: Boolean) = ???

  def blockArrayA(): MultiArrayA[MultiArrayD[X, B, C, D, E], A] =
    MultiArrayA(as, as.map(subArray(_, x => x)).toArray)

  def blockArrayB(): MultiArrayB[MultiArrayC[X, C, D, E], A, B] =
    val arr1 = nested().map(_.map(multiArray(cs, ds, es, _)))
    multiArray(as, bs, arr1)

  def blockArrayC(): MultiArrayC[MultiArrayB[X, D, E], A, B, C] =
    val arr1 = nested().map(_.map(_.map(multiArray(ds, es, _))))
    multiArray(as, bs, cs, arr1)

  def transform[Z](f: X => Z)(using ClassTag[Z]) =
    MultiArrayE(as, bs, cs, ds, es, arr.map(f))

  def transform[Z](f: (X, A, B, C, D, E) => Z)(using ClassTag[Z]) =
    MultiArrayE(
      as,
      bs,
      cs,
      ds,
      es,
      (nested() zip as)
        .map((l1, a) =>
          (l1 zip bs).map((l2, b) =>
            (l2 zip cs).map((l3, c) => (l3 zip ds).map((l4, d) => (l4 zip es).map((x, e) => f(x, a, b, c, d, e))))
          )
        )
        .flatten
        .flatten
        .flatten
        .flatten
        .toArray
    )
