package multiarray

import augmented._
import augmented.given
import basicdef._
import plotutil._
import shape._
import util._
import util.JavaUtil.Pair

import collection.JavaConverters._
import math.Numeric.Implicits.infixNumericOps
import scala.reflect.ClassTag

class ArrayB[X](x: Seq[Seq[X]], y: Array[X]):
  def length: Int = 0
  val orig: Seq[Seq[X]] = x
  val arr: Array[X] = y

trait MultiArrayT:
  def plot(title: String, addTimeStamp: Boolean, name: String, visibleAxes: Boolean): Unit

case class MultiArrayA[X, A](as: Seq[A], x: Seq[X], y: Array[X])(using ClassTag[X]):

  val length = as.length
  val arr: Array[X] = x.toArray
  val orig = x
  val axisLengths = List(1)

  def apply(a: A) =
    val ia = as.indexOf(a)
    val i = getIndex(List(ia), axisLengths, length, List(a))
    arr(i)

  override def toString(): String = s">>> [A:$length]\n${orig}\n\n${arr.mkString(" ")}\n"

object multiArray:

  def apply[X, A, B](as: Seq[A], bs: Seq[B], na: Array[Array[X]])(using ClassTag[X]) =
    MultiArrayB(as, bs, na.map(_.toList).toList, Array[X]())

  def apply[X, A, B](as: Seq[A], bs: Seq[B], na: Seq[Seq[X]])(using ClassTag[X]) =
    MultiArrayB(as, bs, na, Array[X]())

  def apply[X, A, B](as: Seq[A], bs: Seq[B], y: Array[X])(using ClassTag[X]) =
    MultiArrayB(as, bs, Vector[Vector[X]](), y)

  def apply[X](na: Array[Array[X]])(using ClassTag[X]) =
    val l = na.map(_.toList).toList
    if l.length > 0 && isRegular(l) then MultiArrayB(0 until na.length, 0 until na.head.length, l, Array[X]())
    else ???

  def apply[X, A, B, C](as: Seq[A], bs: Seq[B], cs: Seq[C], na: Seq[Seq[Seq[X]]])(using ClassTag[X]) =
    MultiArrayC(as, bs, cs, na, Array[X]())

  def multiarray[X, A, B](as: JList[A], bs: JList[B], na: JList[JList[X]]) =
    val l1 = as.asScala.toList
    val l2 = bs.asScala.toList
    val ln = na.asScala.toList.map(_.asScala.toList)
    given ClassTag[X] = ClassTag(ln.head.head.getClass)
    MultiArrayB(l1, l2, ln, Array[X]())

  def multiarray[X, A, B, C](as: JList[A], bs: JList[B], cs: JList[C], na: JList[JList[JList[X]]]) =
    val l1 = as.asScala.toList
    val l2 = bs.asScala.toList
    val l3 = cs.asScala.toList
    val ln = na.asScala.toList.map(_.asScala.toList.map(_.asScala.toList))
    given ClassTag[X] = ClassTag(ln.head.head.head.getClass)
    MultiArrayC(l1, l2, l3, ln, Array[X]())

  def multiarray[X, A, B, C](as: JList[A], bs: JList[B], cs: JList[C], na: Seq[Seq[Seq[X]]]) =
    val l1 = as.asScala.toList
    val l2 = bs.asScala.toList
    val l3 = cs.asScala.toList
    given ClassTag[X] = ClassTag(na.head.head.head.getClass)
    MultiArrayC(l1, l2, l3, na, Array[X]())

  def apply[X](na: Array[Array[Array[X]]])(using ClassTag[X]) =
    val l = na.map(_.map(_.toList).toList).toList
    if l.length > 0 && l.head.length > 0 && isRegular(l) then
      MultiArrayC(0 until na.length, 0 until na.head.length, 0 until na.head.head.length, l, Array[X]())
    else ???

  def plus[Z, A, B](na: MultiArrayB[Z, A, B], nb: MultiArrayB[Z, A, B])(using
      Numeric[Z],
      ClassTag[Z]
  ): MultiArrayB[Z, A, B] =
    if na.axisLengths != nb.axisLengths then
      throw new Exception(s"Arrays must have same dimensions: ${na.axisLengths} vs ${nb.axisLengths}")
    val ys = (na.arr zip nb.arr).map(_ + _)
    multiArray(na.as, na.bs, ys)

case class MultiArrayB[X, A, B](as: Seq[A], bs: Seq[B], x: Seq[Seq[X]], y: Array[X])(using ClassTag[X])
    extends ArrayB(x, y)
    with MultiArrayT:

  def this(as: Seq[A], bs: Seq[B], y: Array[X])(using ClassTag[X]) =
    this(as, bs, Vector[Vector[X]](), y)

  override val length = as.length * bs.length
  override val arr: Array[X] =
    if y.length > 0 then y else List.concat(x: _*).toArray
  override val orig: Seq[Seq[X]] = if x.length > 0 then x else y.toList.grouped(as.length).toList
  def origAsJava(): JList[JList[X]] = orig.map(_.asJava).asJava

  def flat() = orig.flatMap(_)

  override def toString(): String =
    toString(_.toString)

  def toString(replaceFn: X => String = _.toString) =
    val disp =
      orig.map:
        _.map: x =>
          x match
            case b: Boolean => if b then 1 else 0
            case _          => replaceFn(x)
    "\n" + disp.reverse.map(_.mkString(" ")).mkString("\n") + "\n"

  def apply(a: A, b: B) =
    val ia = as.indexOf(a)
    val ib = bs.indexOf(b)
    val i = getIndex(List(ia, ib), axisLengths, length, List(a, b))
    arr(i)

  def apply(coords: (A, B)): X = apply(coords._1, coords._2)

  def get(a: A, b: B, default: X) =
    val ia = as.indexOf(a)
    val ib = bs.indexOf(b)
    if ia >= 0 && ib >= 0 then
      val i = getIndex(List(ia, ib), axisLengths, length, List(a, b))
      if i >= 0 && i < arr.length then arr(i)
      else default
    else default

  def la = as.length
  def lb = bs.length

  def rows() =
    (0 until bs.length).map(j => (0 until as.length).map(i => arr(j * as.length + i))).toVector.toVector

  def cols() =
    (0 until as.length).map(i => (0 until bs.length).map(j => arr(j * as.length + i))).toVector.toVector

  def diags() =
    if as.length != bs.length then None
    else
      val l = as.length
      Some((as zip bs).map(this(_, _)), (as zip bs.reverse).map(this(_, _)))

  def row(b: B) = rows()(bs.indexOf(b))
  def col(a: A) = cols()(as.indexOf(a))

  def subArray(i: Int, j: Int, la: Int, lb: Int): Seq[Seq[X]] =
    orig.slice(i * la, i * la + lb).map(_.slice(j * lb, j * lb + la))

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

  def select(phi: X => Boolean): Seq[(A, B)] =
    tupleB(as, bs, (a: A, b: B) => phi(this(a, b)))

  def selectPairs(phi: X => Boolean): JList[Pair[A, B]] =
    var f = augment((x: A, y: B) => Pair(x, y))
    f(as.asJava, (a: A) => bs.asJava, (a: A, b: B) => phi(this(a, b)))

  val axisLengths = List(bs.length, 1)

  def plot(title: String = "", addTimeStamp: Boolean = false, name: String = "", visibleAxes: Boolean = true) =
    multiarrayplot.PlotExtensionsB.plot(this)(title, addTimeStamp, name, visibleAxes)

  def plotflat(title: String = "", addTimeStamp: Boolean = false, name: String = "", visibleAxes: Boolean = true) =
    multiarrayplot.PlotExtensionsB.plotflat(this)(title, addTimeStamp, name, visibleAxes)

// A, B, C <-> Z, Y, X

case class MultiArrayC[X, A, B, C](as: Seq[A], bs: Seq[B], cs: Seq[C], x: Seq[Seq[Seq[X]]], y: Array[X])(using
    ClassTag[X]
) extends MultiArrayT:

  val length = as.length * bs.length * cs.length
  def lengths(i: Int) = List(as, bs, cs)(i).length
  val arr: Array[X] = List.concat(List.concat(x: _*): _*).toArray
  val orig = x
  def origAsJava(): JList[JList[JList[X]]] = orig.map(_.map(_.asJava).asJava).asJava

  def flat() = orig.flatMap(_.flatMap(x => x))

  override def toString(): String =
    val disp =
      orig.map:
        _.map:
          _.map: x =>
            x match
              case b: Boolean => if b then 1 else 0
              case _          => x
    "\n" + disp.map(_.reverse.map(_.mkString(" ")).mkString("\n") + "\n").mkString("\n")

  def apply(a: A, b: B, c: C) =
    val ia = as.indexOf(a)
    val ib = bs.indexOf(b)
    val ic = cs.indexOf(c)
    val i = getIndex(List(ia, ib, ic), axisLengths, length, List(a, b, c))
    arr(i)

  val axisLengths = List(bs.length * cs.length, cs.length, 1)

  def subArray(a: A): MultiArrayB[X, B, C] =
    val ia = as.indexOf(a)
    val subSeq = orig(ia)
    multiArray(bs, cs, subSeq)

  def plot(title: String = "", addTimeStamp: Boolean = false, name: String = "", visibleAxes: Boolean = true) =
    multiarrayplot.PlotExtensionsC.plot(this)(title, addTimeStamp, name, visibleAxes)

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

class MultiArrayD[X, A, B, C, D](as: Seq[A], bs: Seq[B], cs: Seq[C], ds: Seq[D], x: Seq[Seq[Seq[Seq[X]]]])(using
    ClassTag[X]
):

  val length = as.length * bs.length * cs.length * ds.length
  val arr: Array[X] = List.concat(List.concat(List.concat(x: _*): _*): _*).toArray
  val orig = x
  def origAsJava(): JList[JList[JList[JList[X]]]] = orig.map(_.map(_.map(_.asJava).asJava).asJava).asJava

  def apply(a: A, b: B, c: C, d: D) =
    val ia = as.indexOf(a)
    val ib = bs.indexOf(b)
    val ic = cs.indexOf(c)
    val id = ds.indexOf(d)
    val i = getIndex(List(ia, ib, ic, id), axisLengths, length, List(a, b, c, d))
    arr(i)

  def map[Z](f: Seq[X] => Z)(using ClassTag[Z]) =
    val sq =
      as.map:
        a => bs.map:
          b =>
            f(
              cs.map:
                c => ds.map:
                  d => this(a, b, c, d)
              .flatten
            )
    multiArray(as, bs, sq)

  def map[Z](f: JList[X] => Z) =
    given ClassTag[Z] = ClassTag(this(as.head, bs.head, cs.head, ds.head).getClass)
    val sq =
      as.map:
        a => bs.map:
          b =>
            f(
              cs.map:
                c => ds.map:
                  d => this(a, b, c, d)
              .flatten.asJava
            )
    multiArray(as, bs, sq)

  val axisLengths = List(bs.length * cs.length * ds.length, cs.length * ds.length, ds.length, 1)

class MultiArrayE[X, A, B, C, D, E](
    as: Seq[A],
    bs: Seq[B],
    cs: Seq[C],
    ds: Seq[D],
    es: Seq[E],
    x: Seq[Seq[Seq[Seq[Seq[X]]]]]
)(using ClassTag[X]):

  val length = as.length * bs.length * cs.length * ds.length * es.length
  val arr: Array[X] = List.concat(List.concat(List.concat(List.concat(x: _*): _*): _*): _*).toArray
  val orig = x
  def origAsJava(): JList[JList[JList[JList[JList[X]]]]] =
    orig.map(_.map(_.map(_.map(_.asJava).asJava).asJava).asJava).asJava

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

  def getSubArrayAt[Z](a: A, b: B, c: C, f: X => Z)(using ClassTag[Z]): MultiArrayB[Z, D, E] =
    val ia = as.indexOf(a)
    val ib = bs.indexOf(b)
    val ic = cs.indexOf(c)
    val subIndexes = Array(ia, ib, ic, 0, 0)
    val ind = getIndex(subIndexes, axisLengths, length, List(a, b, c))
    val sa = arr.slice(ind, ind + ds.length * es.length)
    MultiArrayB(ds, es, Vector[Vector[Z]](), sa.map(f).toArray)

def getIndex(subIndexes: Seq[Int], axisLengths: Seq[Int], l: Int, coords: Seq[Any]) =
  if (subIndexes.contains(-1)) then throw new Exception(s"Element not found: ${coords.mkString(".")}")
  (subIndexes zip axisLengths).map(_ * _).sum
