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

  def flat() = arr.toList
  def axes = List(as)
  val length = axes.map(_.length).product
  val axisLengths = getAxisLengths(axes)
  def head = apply(as.head)
  def isEmpty = axes.exists(_.isEmpty)

  def apply(a: A) =
    val vs = List(a)
    arr(getIndex(axes.lazyZip(vs).map(_ indexOf _), axisLengths, length, vs))

  def transform[Z](f: X => Z)(using ClassTag[Z]) =
    MultiArrayA(as, arr.map(f))

  def transform[Z](f: (X, A) => Z)(using ClassTag[Z]) =
    multiArray(as, (arr zip as).map(f(_, _)))

  def toSeq(): Seq[(A, X)] = (as zip arr)
  def toSeq[Z](f: X => Z)(using ClassTag[Z]): Seq[(A, Z)] = (as zip arr.map(f))

  def plot(title: String, addTimeStamp: Boolean, name: String, visibleAxes: Boolean) = ???

  override def toString(): String = s"[A:$length]\n${arr.mkString(" ")}\n"

def getAxisLengths[A](axes: List[Seq[A]]) =
  val length = axes.map(_.length).product
  (1 to axes.length).map(i => length / (axes.take(i)).map(_.length).product)

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

  def apply[X, A, B, C, D, E, F](
      as: Seq[A],
      bs: Seq[B],
      cs: Seq[C],
      ds: Seq[D],
      es: Seq[E],
      fs: Seq[F],
      na: Seq[Seq[Seq[Seq[Seq[Seq[X]]]]]]
  )(using ClassTag[X]) =
    checkedMultiArray(as, bs, cs, ds, es, fs, na.flatten.flatten.flatten.flatten.flatten.toArray)

  def apply[X, A, B, C, D, E, F](as: Seq[A], bs: Seq[B], cs: Seq[C], ds: Seq[D], es: Seq[E], fs: Seq[F], arr: Array[X])(
      using ClassTag[X]
  ) =
    checkedMultiArray(as, bs, cs, ds, es, fs, arr)

  def apply[X, A, B, C, D, E, F, G](
      as: Seq[A],
      bs: Seq[B],
      cs: Seq[C],
      ds: Seq[D],
      es: Seq[E],
      fs: Seq[F],
      gs: Seq[G],
      na: Seq[Seq[Seq[Seq[Seq[Seq[Seq[X]]]]]]]
  )(using ClassTag[X]) =
    checkedMultiArray(as, bs, cs, ds, es, fs, gs, na.flatten.flatten.flatten.flatten.flatten.flatten.toArray)

  def apply[X, A, B, C, D, E, F, G](
      as: Seq[A],
      bs: Seq[B],
      cs: Seq[C],
      ds: Seq[D],
      es: Seq[E],
      fs: Seq[F],
      gs: Seq[G],
      arr: Array[X]
  )(using ClassTag[X]) =
    checkedMultiArray(as, bs, cs, ds, es, fs, gs, arr)

  def apply[X, A, B, C, D, E, F, G, H](
      as: Seq[A],
      bs: Seq[B],
      cs: Seq[C],
      ds: Seq[D],
      es: Seq[E],
      fs: Seq[F],
      gs: Seq[G],
      hs: Seq[H],
      na: Seq[Seq[Seq[Seq[Seq[Seq[Seq[Seq[X]]]]]]]]
  )(using ClassTag[X]) =
    checkedMultiArray(
      as,
      bs,
      cs,
      ds,
      es,
      fs,
      gs,
      hs,
      na.flatten.flatten.flatten.flatten.flatten.flatten.flatten.toArray
    )

  def apply[X, A, B, C, D, E, F, G, H](
      as: Seq[A],
      bs: Seq[B],
      cs: Seq[C],
      ds: Seq[D],
      es: Seq[E],
      fs: Seq[F],
      gs: Seq[G],
      hs: Seq[H],
      arr: Array[X]
  )(using ClassTag[X]) =
    checkedMultiArray(as, bs, cs, ds, es, fs, gs, hs, arr)

  def apply[X, A, B, C, D, E, F, G, H, I](
      as: Seq[A],
      bs: Seq[B],
      cs: Seq[C],
      ds: Seq[D],
      es: Seq[E],
      fs: Seq[F],
      gs: Seq[G],
      hs: Seq[H],
      is: Seq[I],
      na: Seq[Seq[Seq[Seq[Seq[Seq[Seq[Seq[Seq[X]]]]]]]]]
  )(using ClassTag[X]) =
    checkedMultiArray(
      as,
      bs,
      cs,
      ds,
      es,
      fs,
      gs,
      hs,
      is,
      na.flatten.flatten.flatten.flatten.flatten.flatten.flatten.flatten.toArray
    )

  def apply[X, A, B, C, D, E, F, G, H, I](
      as: Seq[A],
      bs: Seq[B],
      cs: Seq[C],
      ds: Seq[D],
      es: Seq[E],
      fs: Seq[F],
      gs: Seq[G],
      hs: Seq[H],
      is: Seq[I],
      arr: Array[X]
  )(using ClassTag[X]) =
    checkedMultiArray(as, bs, cs, ds, es, fs, gs, hs, is, arr)

  def apply[X, A, B, C, D, E, F, G, H, I, J](
      as: Seq[A],
      bs: Seq[B],
      cs: Seq[C],
      ds: Seq[D],
      es: Seq[E],
      fs: Seq[F],
      gs: Seq[G],
      hs: Seq[H],
      is: Seq[I],
      js: Seq[J],
      arr: Array[X]
  )(using ClassTag[X]) =
    checkedMultiArray(as, bs, cs, ds, es, fs, gs, hs, is, js, arr)

  def apply[X, A, B, C, D, E, F, G, H, I, J, K](
      as: Seq[A],
      bs: Seq[B],
      cs: Seq[C],
      ds: Seq[D],
      es: Seq[E],
      fs: Seq[F],
      gs: Seq[G],
      hs: Seq[H],
      is: Seq[I],
      js: Seq[J],
      ks: Seq[K],
      arr: Array[X]
  )(using ClassTag[X]) =
    checkedMultiArray(as, bs, cs, ds, es, fs, gs, hs, is, js, ks, arr)

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

  def multiarray[X, A, B](as: BaseStream[A, ?], bs: BaseStream[B, ?], na: JList[JList[X]]): MultiArrayB[X, A, B] =
    multiarray(streamToList(as), streamToList(bs), na)

  def multiarray[X, A, B](as: BaseStream[A, ?], bs: BaseStream[B, ?], na: JStream[JStream[X]]): MultiArrayB[X, A, B] =
    val l: JList[JList[X]] = streamToList(na).asScala.map(streamToList).asJava
    multiarray(as, bs, l)

  def multiarray[X, A, B, C](
      as: BaseStream[A, ?],
      bs: BaseStream[B, ?],
      cs: BaseStream[C, ?],
      na: JList[JList[JList[X]]]
  ): MultiArrayC[X, A, B, C] =
    multiarray(streamToList(as), streamToList(bs), streamToList(cs), na)

  def multiarray[X, A, B, C, D <: BaseStream[X, ?]](
      as: BaseStream[A, ?],
      bs: BaseStream[B, ?],
      cs: BaseStream[C, ?],
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

trait ArrayB[X]:
  val arr: Array[X]
  def length: Int = 0

def getIndex(subIndexes: Seq[Int], axisLengths: Seq[Int], l: Int, coords: Seq[Any]) =
  if (subIndexes.contains(-1)) then throw new Exception(s"Element not found: ${coords.mkString(".")}")
  (subIndexes zip axisLengths).map(_ * _).sum

def getIndexOpt(subIndexes: Seq[Int], axisLengths: Seq[Int], l: Int, coords: Seq[Any]) =
  if (subIndexes.contains(-1)) then None
  else Some((subIndexes zip axisLengths).map(_ * _).sum)

def indexOfStrict[A](seq: Seq[A], v: A, ctx: => Any): Int =
  val i = seq.indexOf(v)
  if i == -1 then throw new Exception(s"Index $v not found in $seq\n$ctx")
  i

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
def checkInputDim[X, A, B, C, D, E, F](
    as: Seq[A],
    bs: Seq[B],
    cs: Seq[C],
    ds: Seq[D],
    es: Seq[E],
    fs: Seq[F],
    xs: Seq[X]
) =
  if as.length * bs.length * cs.length * ds.length * es.length * fs.length != xs.length then
    throw new Exception(
      s"Non-rectangular input lengths: ${as.length} ${bs.length} ${cs.length} ${ds.length} ${es.length} ${fs.length} ${xs.length}"
    )
def checkInputDim[X, A, B, C, D, E, F, G](
    as: Seq[A],
    bs: Seq[B],
    cs: Seq[C],
    ds: Seq[D],
    es: Seq[E],
    fs: Seq[F],
    gs: Seq[G],
    xs: Seq[X]
) =
  if as.length * bs.length * cs.length * ds.length * es.length * fs.length * gs.length != xs.length then
    throw new Exception(
      s"Non-rectangular input lengths: ${as.length} ${bs.length} ${cs.length} ${ds.length} ${es.length} ${fs.length} ${gs.length} ${xs.length}"
    )
def checkInputDim[X, A, B, C, D, E, F, G, H](
    as: Seq[A],
    bs: Seq[B],
    cs: Seq[C],
    ds: Seq[D],
    es: Seq[E],
    fs: Seq[F],
    gs: Seq[G],
    hs: Seq[H],
    xs: Seq[X]
) =
  if as.length * bs.length * cs.length * ds.length * es.length * fs.length * gs.length * hs.length != xs.length then
    throw new Exception(
      s"Non-rectangular input lengths: ${as.length} ${bs.length} ${cs.length} ${ds.length} ${es.length} ${fs.length} ${gs.length} ${hs.length} ${xs.length}"
    )
def checkInputDim[X, A, B, C, D, E, F, G, H, I](
    as: Seq[A],
    bs: Seq[B],
    cs: Seq[C],
    ds: Seq[D],
    es: Seq[E],
    fs: Seq[F],
    gs: Seq[G],
    hs: Seq[H],
    is: Seq[I],
    xs: Seq[X]
) =
  if as.length * bs.length * cs.length * ds.length * es.length * fs.length * gs.length * hs.length * is.length != xs.length
  then
    throw new Exception(
      s"Non-rectangular input lengths: ${as.length} ${bs.length} ${cs.length} ${ds.length} ${es.length} ${fs.length} ${gs.length} ${hs.length} ${is.length} ${xs.length}"
    )
def checkInputDim[X, A, B, C, D, E, F, G, H, I, J](
    as: Seq[A],
    bs: Seq[B],
    cs: Seq[C],
    ds: Seq[D],
    es: Seq[E],
    fs: Seq[F],
    gs: Seq[G],
    hs: Seq[H],
    is: Seq[I],
    js: Seq[J],
    xs: Seq[X]
) =
  if as.length * bs.length * cs.length * ds.length * es.length * fs.length * gs.length * hs.length * is.length * js.length != xs.length
  then
    throw new Exception(
      s"Non-rectangular input lengths: ${as.length} ${bs.length} ${cs.length} ${ds.length} ${es.length} ${fs.length} ${gs.length} ${hs.length} ${is.length} ${js.length} ${xs.length}"
    )
def checkInputDim[X, A, B, C, D, E, F, G, H, I, J, K](
    as: Seq[A],
    bs: Seq[B],
    cs: Seq[C],
    ds: Seq[D],
    es: Seq[E],
    fs: Seq[F],
    gs: Seq[G],
    hs: Seq[H],
    is: Seq[I],
    js: Seq[J],
    ks: Seq[K],
    xs: Seq[X]
) =
  if as.length * bs.length * cs.length * ds.length * es.length * fs.length * gs.length * hs.length * is.length * js.length * ks.length != xs.length
  then
    throw new Exception(
      s"Non-rectangular input lengths: ${as.length} ${bs.length} ${cs.length} ${ds.length} ${es.length} ${fs.length} ${gs.length} ${hs.length} ${is.length} ${js.length} ${ks.length} ${xs.length}"
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
def checkedMultiArray[X, A, B, C, D, E, F](
    as: Seq[A],
    bs: Seq[B],
    cs: Seq[C],
    ds: Seq[D],
    es: Seq[E],
    fs: Seq[F],
    xs: Array[X]
)(using ClassTag[X]) =
  checkInputDim(as, bs, cs, ds, es, fs, xs)
  MultiArrayF(as, bs, cs, ds, es, fs, xs)
def checkedMultiArray[X, A, B, C, D, E, F, G](
    as: Seq[A],
    bs: Seq[B],
    cs: Seq[C],
    ds: Seq[D],
    es: Seq[E],
    fs: Seq[F],
    gs: Seq[G],
    xs: Array[X]
)(using ClassTag[X]) =
  checkInputDim(as, bs, cs, ds, es, fs, gs, xs)
  MultiArrayG(as, bs, cs, ds, es, fs, gs, xs)
def checkedMultiArray[X, A, B, C, D, E, F, G, H](
    as: Seq[A],
    bs: Seq[B],
    cs: Seq[C],
    ds: Seq[D],
    es: Seq[E],
    fs: Seq[F],
    gs: Seq[G],
    hs: Seq[H],
    xs: Array[X]
)(using ClassTag[X]) =
  checkInputDim(as, bs, cs, ds, es, fs, gs, hs, xs)
  MultiArrayH(as, bs, cs, ds, es, fs, gs, hs, xs)
def checkedMultiArray[X, A, B, C, D, E, F, G, H, I](
    as: Seq[A],
    bs: Seq[B],
    cs: Seq[C],
    ds: Seq[D],
    es: Seq[E],
    fs: Seq[F],
    gs: Seq[G],
    hs: Seq[H],
    is: Seq[I],
    xs: Array[X]
)(using ClassTag[X]) =
  checkInputDim(as, bs, cs, ds, es, fs, gs, hs, is, xs)
  MultiArrayI(as, bs, cs, ds, es, fs, gs, hs, is, xs)
def checkedMultiArray[X, A, B, C, D, E, F, G, H, I, J](
    as: Seq[A],
    bs: Seq[B],
    cs: Seq[C],
    ds: Seq[D],
    es: Seq[E],
    fs: Seq[F],
    gs: Seq[G],
    hs: Seq[H],
    is: Seq[I],
    js: Seq[J],
    xs: Array[X]
)(using ClassTag[X]) =
  checkInputDim(as, bs, cs, ds, es, fs, gs, hs, is, js, xs)
  MultiArrayJ(as, bs, cs, ds, es, fs, gs, hs, is, js, xs)
def checkedMultiArray[X, A, B, C, D, E, F, G, H, I, J, K](
    as: Seq[A],
    bs: Seq[B],
    cs: Seq[C],
    ds: Seq[D],
    es: Seq[E],
    fs: Seq[F],
    gs: Seq[G],
    hs: Seq[H],
    is: Seq[I],
    js: Seq[J],
    ks: Seq[K],
    xs: Array[X]
)(using ClassTag[X]) =
  checkInputDim(as, bs, cs, ds, es, fs, gs, hs, is, js, ks, xs)
  MultiArrayK(as, bs, cs, ds, es, fs, gs, hs, is, js, ks, xs)

case class MultiArrayB[X, A, B](as: Seq[A], bs: Seq[B], arr: Array[X])(using ClassTag[X])
    extends ArrayB[X]
    with MultiArrayT[X]:

  def this(as: Seq[A], bs: Seq[B], na: Seq[Seq[X]])(using ClassTag[X]) =
    this(as, bs, na.flatten.toArray)

  def flat(): Seq[X] = arr.toList
  def nested(): Seq[Seq[X]] = augment.seq(this(_, _))(as, bs)
  def nestedAsJava(): JList[JList[X]] = nested().map(_.asJava).asJava

  def axes = List(as, bs)
  override val length = axes.map(_.length).product
  val axisLengths = getAxisLengths(axes)
  def head = apply(as.head, bs.head)
  def isEmpty = axes.exists(_.isEmpty)

  def apply(a: A, b: B) =
    val vs = List(a, b)
    arr(getIndex(axes.lazyZip(vs).map(_ indexOf _), axisLengths, length, vs))

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

  infix def select(phi: X => Boolean): Seq[(A, B)] =
    tupleB(as, bs, (a: A, b: B) => phi(this(a, b)))

  infix def selectPairs(phi: X => Boolean): JList[Pair[A, B]] =
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
        if !x.isInstanceOf[Map[?, ?]] then
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
        if !x.isInstanceOf[Map[?, ?]] then
          throw new Exception(s"Expected values of type map but found: ${x.getClass}:\n$x\n")
        val m = x.asInstanceOf[Map[C, Y]]
        cs.map(c => m.getOrElse(c, altC(c)))
      ))
    )

  def transpose(): MultiArrayB[X, B, A] =
    image(bs, as, (b, a) => this(a, b))

  def plot(title: String = "", addTimeStamp: Boolean = false, name: String = "", visibleAxes: Boolean = true) =
    multiarrayplot.PlotExtensionsB.plotB(this)(title, addTimeStamp, name, visibleAxes)

  def plotFlat(title: String = "", addTimeStamp: Boolean = false, name: String = "", visibleAxes: Boolean = true) =
    multiarrayplot.PlotExtensionsB.plotFlatB(this)(title, addTimeStamp, name, visibleAxes)

  def plotContour(title: String = "", addTimeStamp: Boolean = false, name: String = "", visibleAxes: Boolean = true) =
    multiarrayplot.PlotExtensionsB.plotContourB(this)(title, addTimeStamp, name, visibleAxes)

  def plotHeatMap(title: String = "", addTimeStamp: Boolean = false, name: String = "", visibleAxes: Boolean = true) =
    multiarrayplot.PlotExtensionsB.plotHeatMapB(this)(title, addTimeStamp, name, visibleAxes)

  def graph() = multiarrayplot.PlotExtensionsB.graphB(this)()

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

/** A, B, C <-> Z, Y, X */

case class MultiArrayC[X, A, B, C](as: Seq[A], bs: Seq[B], cs: Seq[C], arr: Array[X])(using
    ClassTag[X]
) extends MultiArrayT[X]:

  def lengths(i: Int) = List(as, bs, cs)(i).length
  def flat(): Seq[X] = arr.toList
  def nested(): Seq[Seq[Seq[X]]] = augment.seq(this(_, _, _))(as, bs, cs)
  def nestedAsJava(): JList[JList[JList[X]]] = nested().map(_.map(_.asJava).asJava).asJava

  def axes = List(as, bs, cs)
  val length = axes.map(_.length).product
  val axisLengths = getAxisLengths(axes)
  def head = apply(as.head, bs.head, cs.head)
  def isEmpty = axes.exists(_.isEmpty)

  def apply(a: A, b: B, c: C): X =
    val vs = List(a, b, c)
    arr(getIndex(axes.lazyZip(vs).map(_ indexOf _), axisLengths, length, vs))

  def get(a: A, b: B, c: C): Option[X] =
    val vs = List(a, b, c)
    getIndexOpt(axes.lazyZip(vs).map(_ indexOf _), axisLengths, length, vs).map(arr(_))

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
    multiarrayplot.PlotExtensionsC.plotC(this)(title, addTimeStamp, name, visibleAxes)

  def graph() = multiarrayplot.PlotExtensionsC.graphC(this)()

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
      .animateC(this)(title, flat, duration, addTimeStamp, name, visibleAxes, imageSources, getGridColors)

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
      .animateC(this)(title, flat, duration, false, "", visibleAxes, imageSources, (a, b) => Color(getGridColors(a, b)))

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

  def flat(): Seq[X] = arr.toList
  def nested(): Seq[Seq[Seq[Seq[X]]]] = augment.seq(this(_, _, _, _))(as, bs, cs, ds)
  def nestedAsJava(): JList[JList[JList[JList[X]]]] = nested().map(_.map(_.map(_.asJava).asJava).asJava).asJava

  def axes = List(as, bs, cs, ds)
  val length = axes.map(_.length).product
  val axisLengths = getAxisLengths(axes)
  def head = apply(as.head, bs.head, cs.head, ds.head)
  def isEmpty = axes.exists(_.isEmpty)

  def apply(a: A, b: B, c: C, d: D) =
    val vs = List(a, b, c, d)
    arr(getIndex(axes.lazyZip(vs).map(_ indexOf _), axisLengths, length, vs))

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

  def subArray[Z](a: A, f: X => Z)(using ClassTag[Z]): MultiArrayC[Z, B, C, D] =
    val ia = as.indexOf(a)
    if ia == -1 then throw new Exception(s"Index $a not found in ${as}\n${this}")
    val subIndexes = Array(ia, 0, 0, 0, 0)
    val ind = getIndex(subIndexes, axisLengths, length, List(a))
    val arr1 = arr.slice(ind, ind + axisLengths(0))
    multiArray(bs, cs, ds, arr1.map(f))

  def subArray[Z](a: A, b: B, f: X => Z)(using ClassTag[Z]): MultiArrayB[Z, C, D] =
    val vs = List(a, b)
    val subIndexes = axes.lazyZip(vs).map(indexOfStrict(_, _, this))
    val ind = getIndex(subIndexes, axisLengths, length, vs)
    val arr1 = arr.slice(ind, ind + axisLengths(vs.size - 1))
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

  def flat(): Seq[X] = arr.toList
  def nested(): Seq[Seq[Seq[Seq[Seq[X]]]]] = augment.seq(this(_, _, _, _, _))(as, bs, cs, ds, es)
  def nestedAsJava(): JList[JList[JList[JList[JList[X]]]]] =
    nested().map(_.map(_.map(_.map(_.asJava).asJava).asJava).asJava).asJava

  def axes = List(as, bs, cs, ds, es)
  val length = axes.map(_.length).product
  val axisLengths = getAxisLengths(axes)
  def head = apply(as.head, bs.head, cs.head, ds.head, es.head)
  def isEmpty = axes.exists(_.isEmpty)

  def apply(a: A, b: B, c: C, d: D, e: E) =
    val vs = List(a, b, c, d, e)
    arr(getIndex(axes.lazyZip(vs).map(_ indexOf _), axisLengths, length, vs))

  def subArray[Z](a: A, f: X => Z)(using ClassTag[Z]): MultiArrayD[Z, B, C, D, E] =
    val vs = List(a)
    val subIndexes = axes.lazyZip(vs).map(indexOfStrict(_, _, this))
    val ind = getIndex(subIndexes, axisLengths, length, vs)
    val arr1 = arr.slice(ind, ind + axisLengths(vs.size - 1))
    multiArray(bs, cs, ds, es, arr1.map(f))

  def subArray[Z](a: A, b: B, f: X => Z)(using ClassTag[Z]): MultiArrayC[Z, C, D, E] =
    val vs = List(a, b)
    val subIndexes = axes.lazyZip(vs).map(indexOfStrict(_, _, this))
    val ind = getIndex(subIndexes, axisLengths, length, vs)
    val arr1 = arr.slice(ind, ind + axisLengths(vs.size - 1))
    multiArray(cs, ds, es, arr1.map(f))

  def subArray[Z](a: A, b: B, c: C, f: X => Z)(using ClassTag[Z]): MultiArrayB[Z, D, E] =
    val vs = List(a, b, c)
    val subIndexes = axes.lazyZip(vs).map(indexOfStrict(_, _, this))
    val ind = getIndex(subIndexes, axisLengths, length, vs)
    val arr1 = arr.slice(ind, ind + axisLengths(vs.size - 1))
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
