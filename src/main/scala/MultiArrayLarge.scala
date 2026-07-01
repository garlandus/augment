package multiarray

import augmented._
import augmented.given
import basicdef._
import util.floatToSt

import collection.JavaConverters._
import math.Numeric.Implicits.infixNumericOps
import scala.reflect.ClassTag

case class MultiArrayF[X, A, B, C, D, E, F](
    as: Seq[A],
    bs: Seq[B],
    cs: Seq[C],
    ds: Seq[D],
    es: Seq[E],
    fs: Seq[F],
    arr: Array[X]
)(using ClassTag[X])
    extends MultiArrayT[X]:

  def flat(): Seq[X] = arr.toList
  def nested(): Seq[Seq[Seq[Seq[Seq[Seq[X]]]]]] = augment.seq(this(_, _, _, _, _, _))(as, bs, cs, ds, es, fs)
  def nestedAsJava(): JList[JList[JList[JList[JList[JList[X]]]]]] =
    nested().map(_.map(_.map(_.map(_.map(_.asJava).asJava).asJava).asJava).asJava).asJava

  def axes = List(as, bs, cs, ds, es, fs)
  val length = axes.map(_.length).product
  val axisLengths = getAxisLengths(axes)
  def head = apply(as.head, bs.head, cs.head, ds.head, es.head, fs.head)
  def isEmpty = axes.exists(_.isEmpty)
  def plot(title: String, addTimeStamp: Boolean, name: String, visibleAxes: Boolean) = ???

  def apply(a: A, b: B, c: C, d: D, e: E, f: F) =
    val vs = List(a, b, c, d, e, f)
    arr(getIndex(axes.lazyZip(vs).map(_ indexOf _), axisLengths, length, vs))

  def subArray[Z](a: A, b: B, c: C, d: D, f: X => Z)(using ClassTag[Z]): MultiArrayB[Z, E, F] =
    val vs = List(a, b, c, d)
    val subIndexes = axes.lazyZip(vs).map(_ indexOf _)
    val ind = getIndex(subIndexes, axisLengths, length, vs)
    val sa = arr.slice(ind, ind + axisLengths(vs.size - 1))
    MultiArrayB(es, fs, sa.map(f))

  def subArray[Z](a: A, b: B, c: C, f: X => Z)(using ClassTag[Z]): MultiArrayC[Z, D, E, F] =
    val vs = List(a, b, c)
    val subIndexes = axes.lazyZip(vs).map(_ indexOf _)
    val ind = getIndex(subIndexes, axisLengths, length, vs)
    val sa = arr.slice(ind, ind + axisLengths(vs.size - 1))
    MultiArrayC(ds, es, fs, sa.map(f))

  def subArray[Z](a: A, b: B, f: X => Z)(using ClassTag[Z]): MultiArrayD[Z, C, D, E, F] =
    val vs = List(a, b)
    val subIndexes = axes.lazyZip(vs).map(_ indexOf _)
    val ind = getIndex(subIndexes, axisLengths, length, vs)
    val sa = arr.slice(ind, ind + axisLengths(vs.size - 1))
    MultiArrayD(cs, ds, es, fs, sa.map(f))

  def blockArrayB(): MultiArrayB[MultiArrayD[X, C, D, E, F], A, B] =
    val arr = nested().map(_.map(multiArray(cs, ds, es, fs, _)))
    multiArray(as, bs, arr)

  def blockArrayC(): MultiArrayC[MultiArrayC[X, D, E, F], A, B, C] =
    val arr = nested().map(_.map(_.map(multiArray(ds, es, fs, _))))
    multiArray.apply(as, bs, cs, arr)

  def blockArrayD(): MultiArrayD[MultiArrayB[X, E, F], A, B, C, D] =
    val arr = nested().map(_.map(_.map(_.map(multiArray(es, fs, _)))))
    multiArray(as, bs, cs, ds, arr)

  def transform[Z](f: X => Z)(using ClassTag[Z]) =
    MultiArrayF(as, bs, cs, ds, es, fs, arr.map(f))

  def transform[Z](f: (X, A, B, C, D, E, F) => Z)(using ClassTag[Z]) =
    val r0 =
      (nested() zip as).map((l1, a) =>
        (l1 zip bs).map((l2, b) =>
          (l2 zip cs).map((l3, c) =>
            (l3 zip ds).map((l4, d) => (l4 zip es).map((l5, e) => (l5 zip fs).map((x, f0) => f(x, a, b, c, d, e, f0))))
          )
        )
      )
    MultiArrayF(as, bs, cs, ds, es, fs, r0.flatten.flatten.flatten.flatten.flatten.toArray)

  def getSubArraysB[Z](): MultiArrayD[MultiArrayB[X, E, F], A, B, C, D] =
    val seqs = nested().map(_.map(_.map(_.map(x => multiArray(es, fs, x)))))
    MultiArrayD(as, bs, cs, ds, seqs.flatten.flatten.flatten.toArray)

  def getEFavgs[Z](using Numeric[X]): MultiArrayD[Float, A, B, C, D] =
    getSubArraysB().transform(a => a.arr.sum.toFloat / a.arr.length)

  def getEFpoints[Z](e: E, f: F): MultiArrayD[X, A, B, C, D] =
    getSubArraysB().transform(a => a.get(e, f).get)

  def getCDpoints[Z](c: C, d: D): MultiArrayD[X, A, B, E, F] =
    val a0: MultiArrayB[MultiArrayD[X, C, D, E, F], A, B] =
      multiArray(
        as,
        bs,
        nested().map(_.map(x => MultiArrayD[X, C, D, E, F](cs, ds, es, fs, x.flatten.flatten.flatten.toArray)))
      )
    val a1 = a0.transform(_.subArray(c, d, x => x))
    MultiArrayD(as, bs, es, fs, a1.nested().map(_.map(_.nested())).flatten.flatten.flatten.toArray)

  def saveToFile(
      folder: String,
      filePrefix: String,
      fileSuffix: String,
      hasMultiLineEntry: Boolean = false,
      entrySeparator: String = entryDividingLine,
      entryToSt: X => String = _.toString
  ) =
    val s0 = s"${axes.map(_.mkString("|")).mkString("\n")}\n$entryDividingLine\n"
    val separator = if hasMultiLineEntry then entrySeparator + "\n" else ""
    val s = arr.map(entryToSt).mkString(s"\n$separator")
    util.saveToFile(folder, filePrefix + fileSuffix, s"$s0$s\n", "array")

    def f(a: A, b: B, c: C, d: D) =
      val sa = subArray(a, b, c, d, x => x)
      val fileNm = s"${filePrefix}_${a}_${b}_${c}_${d}.txt"
      sa.saveToFile(folder, fileNm, hasMultiLineEntry, entrySeparator, entryToSt)
    augment(f)(as, bs, cs, ds)

  override def toString(): String = toStr()

  def toStr(xToStr: X => String = _.toString): String =
    (nested() zip as).map((x, a)  => s"$a\n" +
    (x zip bs).map((y, b)         => s"  $b\n" +
    (y zip cs).map((z, c)         => s"    $c\n" + 
    (z zip ds).map((x1, d)        => s"      $d\n" + 
    (x1 zip es).map((y1, e)       => s"        $e\n          " + 
    (y1 zip fs).map((z1, f)       => s"$f ${xToStr(z1)}  ").mkString("\n          "))
    .mkString("\n")).mkString("\n")).mkString("\n")).mkString("\n")).mkString("\n")

case class MultiArrayG[X, A, B, C, D, E, F, G](
    as: Seq[A],
    bs: Seq[B],
    cs: Seq[C],
    ds: Seq[D],
    es: Seq[E],
    fs: Seq[F],
    gs: Seq[G],
    arr: Array[X]
)(using ClassTag[X])
    extends MultiArrayT[X]:

  def flat(): Seq[X] = arr.toList
  def nested(): Seq[Seq[Seq[Seq[Seq[Seq[Seq[X]]]]]]] =
    augment.seq(this(_, _, _, _, _, _, _))(as, bs, cs, ds, es, fs, gs)
  def nestedAsJava(): JList[JList[JList[JList[JList[JList[JList[X]]]]]]] =
    nested().map(_.map(_.map(_.map(_.map(_.map(_.asJava).asJava).asJava).asJava).asJava).asJava).asJava

  def axes = List(as, bs, cs, ds, es, fs, gs)
  val length = axes.map(_.length).product
  val axisLengths = getAxisLengths(axes)
  def head = apply(as.head, bs.head, cs.head, ds.head, es.head, fs.head, gs.head)
  def isEmpty = axes.exists(_.isEmpty)
  def plot(title: String, addTimeStamp: Boolean, name: String, visibleAxes: Boolean) = ???

  def apply(a: A, b: B, c: C, d: D, e: E, f: F, g: G) =
    val vs = List(a, b, c, d, e, f, g)
    arr(getIndex(axes.lazyZip(vs).map(_ indexOf _), axisLengths, length, vs))

  def checkSubIndexes(indexes: Array[Int]) =
    if indexes.exists(_ < 0) then throw Exception(s"Index not found:\n${this}")

  def checkSubIndexes(indexes: Array[Int], coords: Seq[Any]) =
    if indexes.exists(_ < 0) then
      throw Exception(s"Index not found for ${coords.mkString(" ")}:\n${this}\n${coords.mkString(" ")}")

  def subArray[Z](a: A, b: B, c: C, d: D, e: E, f: X => Z)(using ClassTag[Z]): MultiArrayB[Z, F, G] =
    val vs = List(a, b, c, d, e)
    val subIndexes = axes.lazyZip(vs).map(_ indexOf _).toArray
    checkSubIndexes(subIndexes, vs)
    val ind = getIndex(subIndexes, axisLengths, length, vs)
    val sa = arr.slice(ind, ind + axisLengths(vs.size - 1))
    MultiArrayB(fs, gs, sa.map(f))

  def subArray[Z](a: A, b: B, c: C, f: X => Z)(using ClassTag[Z]): MultiArrayD[Z, D, E, F, G] =
    val vs = List(a, b, c)
    val subIndexes = axes.lazyZip(vs).map(_ indexOf _).toArray
    checkSubIndexes(subIndexes, vs)
    val ind = getIndex(subIndexes, axisLengths, length, vs)
    val sa = arr.slice(ind, ind + axisLengths(vs.size - 1))
    MultiArrayD(ds, es, fs, gs, sa.map(f))

  def getSubArraysB[Z](): MultiArrayE[MultiArrayB[X, F, G], A, B, C, D, E] =
    val seqs = nested().map(_.map(_.map(_.map(_.map(x => multiArray(fs, gs, x))))))
    MultiArrayE(as, bs, cs, ds, es, seqs.flatten.flatten.flatten.flatten.toArray)

  def getSubArraysEFG[Z](): MultiArrayD[MultiArrayC[X, E, F, G], A, B, C, D] =
    val seqs = nested().map(_.map(_.map(_.map(x => multiArray(es, fs, gs, x)))))
    MultiArrayD(as, bs, cs, ds, seqs.flatten.flatten.flatten.toArray)

  def getEFavgs[Z](using Numeric[X]): MultiArrayD[Float, A, B, C, D] =
    getSubArraysEFG().transform(a => a.arr.sum.toFloat / a.arr.length)

  def getEFGpoints[Z](e: E, f: F, g: G): MultiArrayD[X, A, B, C, D] =
    getSubArraysEFG().transform(_.get(e, f, g).get)

  def blockArrayB(): MultiArrayB[MultiArrayE[X, C, D, E, F, G], A, B] =
    val arr = nested().map(_.map(multiArray(cs, ds, es, fs, gs, _)))
    multiArray(as, bs, arr)

  def blockArrayC(): MultiArrayC[MultiArrayD[X, D, E, F, G], A, B, C] =
    val arr = nested().map(_.map(_.map(multiArray(ds, es, fs, gs, _))))
    multiArray.apply(as, bs, cs, arr)

  def blockArrayD(): MultiArrayD[MultiArrayC[X, E, F, G], A, B, C, D] =
    val arr = nested().map(_.map(_.map(_.map(multiArray(es, fs, gs, _)))))
    multiArray(as, bs, cs, ds, arr)

  def blockArrayE(): MultiArrayE[MultiArrayB[X, F, G], A, B, C, D, E] =
    val arr = nested().map(_.map(_.map(_.map(_.map(multiArray(fs, gs, _))))))
    multiArray(as, bs, cs, ds, es, arr)

  def transform[Z](f: X => Z)(using ClassTag[Z]) =
    MultiArrayG(as, bs, cs, ds, es, fs, gs, arr.map(f))

  def transform[Z](f: (X, A, B, C, D, E, F, G) => Z)(using ClassTag[Z]) =
    val r0 =
      (nested() zip as).map((l1, a) =>
        (l1 zip bs).map((l2, b) =>
          (l2 zip cs).map((l3, c) =>
            (l3 zip ds).map((l4, d) =>
              (l4 zip es).map((l5, e) =>
                (l5 zip fs).map((l6, f0) => (l6 zip gs).map((x, g) => f(x, a, b, c, d, e, f0, g)))
              )
            )
          )
        )
      )
    MultiArrayG(as, bs, cs, ds, es, fs, gs, r0.flatten.flatten.flatten.flatten.flatten.flatten.toArray)

  def saveToFile(
      folder: String,
      filePrefix: String,
      fileSuffix: String,
      hasMultiLineEntry: Boolean = false,
      entrySeparator: String = entryDividingLine,
      entryToSt: X => String = _.toString
  ) =
    val s0 = s"${axes.map(_.mkString("|")).mkString("\n")}\n$entryDividingLine\n"
    val separator = if hasMultiLineEntry then entrySeparator + "\n" else ""
    val s = arr.map(entryToSt).mkString(s"\n$separator")
    util.saveToFile(folder, filePrefix + fileSuffix, s"$s0$s\n", "array")

    def toStr[A](a: A) =
      a match
        case f: Float => floatToSt(f)
        case _        => a.toString

    def f(a: A, b: B, c: C, d: D, e: E) =
      val sa = subArray(a, b, c, d, e, x => x)
      val fileNm = s"${filePrefix}_${toStr(a)}_${toStr(b)}_${toStr(c)}_${toStr(d)}_${toStr(e)}.txt"
      sa.saveToFile(folder, fileNm, hasMultiLineEntry, entrySeparator, entryToSt)
    augment(f)(as, bs, cs, ds, es)

  override def toString(): String = toStr()

  def toStr(xToStr: X => String = _.toString): String =
    (nested() zip as).map((x, a)  => s"$a\n" +
    (x zip bs).map((y, b)         => s"  $b\n" +
    (y zip cs).map((z, c)         => s"    $c\n" + 
    (z zip ds).map((x1, d)        => s"      $d\n" + 
    (x1 zip es).map((y1, e)       => s"        $e\n" + 
    (y1 zip fs).map((z1, f)       => s"          $f\n            " + 
    (z1 zip gs).map((z2, g)       => s"$g ${xToStr(z2)}  ").mkString("\n          "))
    .mkString("\n")).mkString("\n")).mkString("\n")).mkString("\n")).mkString("\n")).mkString("\n")

case class MultiArrayH[X, A, B, C, D, E, F, G, H](
    as: Seq[A],
    bs: Seq[B],
    cs: Seq[C],
    ds: Seq[D],
    es: Seq[E],
    fs: Seq[F],
    gs: Seq[G],
    hs: Seq[H],
    arr: Array[X]
)(using ClassTag[X])
    extends MultiArrayT[X]:

  def flat(): Seq[X] = arr.toList
  def nested(): Seq[Seq[Seq[Seq[Seq[Seq[Seq[Seq[X]]]]]]]] =
    augment.seq(this(_, _, _, _, _, _, _, _))(as, bs, cs, ds, es, fs, gs, hs)
  def nestedAsJava(): JList[JList[JList[JList[JList[JList[JList[JList[X]]]]]]]] =
    nested().map(_.map(_.map(_.map(_.map(_.map(_.map(_.asJava).asJava).asJava).asJava).asJava).asJava).asJava).asJava

  def axes = List(as, bs, cs, ds, es, fs, gs, hs)
  val length = axes.map(_.length).product
  val axisLengths = getAxisLengths(axes)
  def head = apply(as.head, bs.head, cs.head, ds.head, es.head, fs.head, gs.head, hs.head)
  def isEmpty = axes.exists(_.isEmpty)
  def plot(title: String, addTimeStamp: Boolean, name: String, visibleAxes: Boolean) = ???

  def apply(a: A, b: B, c: C, d: D, e: E, f: F, g: G, h: H) =
    val vs = List(a, b, c, d, e, f, g, h)
    arr(getIndex(axes.lazyZip(vs).map(_ indexOf _), axisLengths, length, vs))

  def subArray[Z](a: A, b: B, c: C, d: D, e: E, f0: F, f: X => Z)(using ClassTag[Z]): MultiArrayB[Z, G, H] =
    val vs = List(a, b, c, d, e, f0)
    val subIndexes = axes.lazyZip(vs).map(_ indexOf _)
    val ind = getIndex(subIndexes, axisLengths, length, vs)
    val sa = arr.slice(ind, ind + axisLengths(vs.size - 1))
    MultiArrayB(gs, hs, sa.map(f))

  def blockArrayC(): MultiArrayC[MultiArrayE[X, D, E, F, G, H], A, B, C] =
    val arr = nested().map(_.map(_.map(multiArray(ds, es, fs, gs, hs, _))))
    multiArray(as, bs, cs, arr)

  def transform[Z](f: X => Z)(using ClassTag[Z]) =
    MultiArrayH(as, bs, cs, ds, es, fs, gs, hs, arr.map(f))

  def transform[Z](f: (X, A, B, C, D, E, F, G, H) => Z)(using ClassTag[Z]) =
    val r0 =
      (nested() zip as).map((l1, a) =>
        (l1 zip bs).map((l2, b) =>
          (l2 zip cs).map((l3, c) =>
            (l3 zip ds).map((l4, d) =>
              (l4 zip es).map((l5, e) =>
                (l5 zip fs).map((l6, f0) =>
                  (l6 zip gs).map((l7, g) => (l7 zip hs).map((x, h) => f(x, a, b, c, d, e, f0, g, h)))
                )
              )
            )
          )
        )
      )
    MultiArrayH(as, bs, cs, ds, es, fs, gs, hs, r0.flatten.flatten.flatten.flatten.flatten.flatten.flatten.toArray)

  def toStr(xToStr: X => String = _.toString): String =
    (nested() zip as).map((x1, a) => s"  $a\n" +
    (x1 zip bs).map((x2, b) => s"    $b\n" +
    (x2 zip cs).map((x3, c) => s"      $c\n" +
    (x3 zip ds).map((x4, d) => s"        $d\n" +
    (x4 zip es).map((x5, e) => s"          $e\n" +
    (x5 zip fs).map((x6, f) => s"            $f\n" +
    (x6 zip gs).map((x7, g) => s"              $g\n" +
    (x7 zip hs).map((x8, h) => s"$h ${xToStr(x8)}  ").mkString("\n          ")
    ).mkString("\n")).mkString("\n")).mkString("\n")).mkString("\n")).mkString("\n")).mkString("\n")).mkString("\n")

case class MultiArrayI[X, A, B, C, D, E, F, G, H, I](
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
)(using ClassTag[X])
    extends MultiArrayT[X]:

  def flat(): Seq[X] = arr.toList
  def nested(): Seq[Seq[Seq[Seq[Seq[Seq[Seq[Seq[Seq[X]]]]]]]]] =
    augment.seq(this(_, _, _, _, _, _, _, _, _))(as, bs, cs, ds, es, fs, gs, hs, is)
  def nestedAsJava(): JList[JList[JList[JList[JList[JList[JList[JList[JList[X]]]]]]]]] =
    nested()
      .map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.asJava).asJava).asJava).asJava).asJava).asJava).asJava).asJava)
      .asJava

  def axes = List(as, bs, cs, ds, es, fs, gs, hs, is)
  val length = axes.map(_.length).product
  val axisLengths = getAxisLengths(axes)
  def head = apply(as.head, bs.head, cs.head, ds.head, es.head, fs.head, gs.head, hs.head, is.head)
  def isEmpty = axes.exists(_.isEmpty)
  def plot(title: String, addTimeStamp: Boolean, name: String, visibleAxes: Boolean) = ???

  def apply(a: A, b: B, c: C, d: D, e: E, f: F, g: G, h: H, i: I) =
    val vs = List(a, b, c, d, e, f, g, h, i)
    arr(getIndex(axes.lazyZip(vs).map(_ indexOf _), axisLengths, length, vs))

  def subArray[Z](a: A, b: B, c: C, d: D, e: E, f0: F, g: G, f: X => Z)(using ClassTag[Z]): MultiArrayB[Z, H, I] =
    val vs = List(a, b, c, d, e, f0, g)
    val subIndexes = axes.lazyZip(vs).map(_ indexOf _)
    val ind = getIndex(subIndexes, axisLengths, length, vs)
    val sa = arr.slice(ind, ind + axisLengths(vs.size - 1))
    MultiArrayB(hs, is, sa.map(f))

  def transform[Z](f: X => Z)(using ClassTag[Z]) =
    MultiArrayI(as, bs, cs, ds, es, fs, gs, hs, is, arr.map(f))

  def transform[Z](f: (X, A, B, C, D, E, F, G, H, I) => Z)(using ClassTag[Z]) =
    val r0 =
      (nested() zip as).map((l1, a) =>
        (l1 zip bs).map((l2, b) =>
          (l2 zip cs).map((l3, c) =>
            (l3 zip ds).map((l4, d) =>
              (l4 zip es).map((l5, e) =>
                (l5 zip fs).map((l6, f0) =>
                  (l6 zip gs).map((l7, g) =>
                    (l7 zip hs).map((l8, h) => (l8 zip is).map((x, i) => f(x, a, b, c, d, e, f0, g, h, i)))
                  )
                )
              )
            )
          )
        )
      )
    MultiArrayI(
      as,
      bs,
      cs,
      ds,
      es,
      fs,
      gs,
      hs,
      is,
      r0.flatten.flatten.flatten.flatten.flatten.flatten.flatten.flatten.toArray
    )

  def toStr(xToStr: X => String = _.toString): String =
    (nested() zip as).map((x1, a) => s"  $a\n" + 
    (x1 zip bs).map((x2, b) => s"    $b\n" + 
    (x2 zip cs).map((x3, c) => s"      $c\n" + 
    (x3 zip ds).map((x4, d) => s"        $d\n" + 
    (x4 zip es).map((x5, e) => s"          $e\n" + 
    (x5 zip fs).map((x6, f) => s"            $f\n" + 
    (x6 zip gs).map((x7, g) => s"              $g\n" + 
    (x7 zip hs).map((x8, h) => s"                $h\n" + 
    (x8 zip is).map((x9, i) => s"$i ${xToStr (x9)}  ").mkString("\n          ")
    ).mkString("\n")).mkString("\n")).mkString("\n")).mkString("\n")).mkString("\n")).mkString("\n")).mkString("\n")).mkString("\n")

case class MultiArrayJ[X, A, B, C, D, E, F, G, H, I, J](
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
)(using ClassTag[X])
    extends MultiArrayT[X]:

  def flat(): Seq[X] = arr.toList
  def nested(): Seq[Seq[Seq[Seq[Seq[Seq[Seq[Seq[Seq[Seq[X]]]]]]]]]] =
    augment.seq(this(_, _, _, _, _, _, _, _, _, _))(as, bs, cs, ds, es, fs, gs, hs, is, js)
  def nestedAsJava(): JList[JList[JList[JList[JList[JList[JList[JList[JList[JList[X]]]]]]]]]] =
    nested()
      .map(
        _.map(
          _.map(_.map(_.map(_.map(_.map(_.map(_.map(_.asJava).asJava).asJava).asJava).asJava).asJava).asJava).asJava
        ).asJava
      )
      .asJava

  def axes = List(as, bs, cs, ds, es, fs, gs, hs, is, js)
  val length = axes.map(_.length).product
  val axisLengths = getAxisLengths(axes)
  def head = apply(as.head, bs.head, cs.head, ds.head, es.head, fs.head, gs.head, hs.head, is.head, js.head)
  def isEmpty = axes.exists(_.isEmpty)
  def plot(title: String, addTimeStamp: Boolean, name: String, visibleAxes: Boolean) = ???

  def apply(a: A, b: B, c: C, d: D, e: E, f: F, g: G, h: H, i: I, j: J) =
    val vs = List(a, b, c, d, e, f, g, h, i, j)
    arr(getIndex(axes.lazyZip(vs).map(_ indexOf _), axisLengths, length, vs))

  def subArray[Z](a: A, b: B, c: C, d: D, e: E, f0: F, g: G, h: H, f: X => Z)(using
      ClassTag[Z]
  ): MultiArrayB[Z, I, J] =
    val vs = List(a, b, c, d, e, f0, g, h)
    val subIndexes = axes.lazyZip(vs).map(_ indexOf _)
    val ind = getIndex(subIndexes, axisLengths, length, vs)
    val sa = arr.slice(ind, ind + axisLengths(vs.size - 1))
    MultiArrayB(is, js, sa.map(f))

  def transform[Z](f: X => Z)(using ClassTag[Z]) =
    MultiArrayJ(as, bs, cs, ds, es, fs, gs, hs, is, js, arr.map(f))

  def transform[Z](f: (X, A, B, C, D, E, F, G, H, I, J) => Z)(using ClassTag[Z]) =
    val r0 =
      (nested() zip as).map((l1, a) =>
        (l1 zip bs).map((l2, b) =>
          (l2 zip cs).map((l3, c) =>
            (l3 zip ds).map((l4, d) =>
              (l4 zip es).map((l5, e) =>
                (l5 zip fs).map((l6, f0) =>
                  (l6 zip gs).map((l7, g) =>
                    (l7 zip hs).map((l8, h) =>
                      (l8 zip is).map((l9, i) => (l9 zip js).map((x, j) => f(x, a, b, c, d, e, f0, g, h, i, j)))
                    )
                  )
                )
              )
            )
          )
        )
      )
    MultiArrayJ(as, bs, cs, ds, es, fs, gs, hs, is, js, r0.flatten.flatten.flatten.flatten.flatten.flatten.flatten.flatten.flatten.toArray)

  def toStr(xToStr: X => String = _.toString): String =
    (nested() zip as).map((x1, a) => s"  $a\n" +
    (x1 zip bs).map((x2, b) => s"    $b\n" +
    (x2 zip cs).map((x3, c) => s"      $c\n" +
    (x3 zip ds).map((x4, d) => s"        $d\n" +
    (x4 zip es).map((x5, e) => s"          $e\n" +
    (x5 zip fs).map((x6, f) => s"            $f\n" +
    (x6 zip gs).map((x7, g) => s"              $g\n" +
    (x7 zip hs).map((x8, h) => s"                $h\n" +
    (x8 zip is).map((x9, i) => s"                  $i\n" +
    (x9 zip js).map((x10, j)  => s"$j ${xToStr (x10)}  ").mkString("\n          ")
    ).mkString("\n")).mkString("\n")).mkString("\n")).mkString("\n")).mkString("\n")).mkString("\n")).mkString("\n")).mkString("\n")).mkString("\n")

case class MultiArrayK[X, A, B, C, D, E, F, G, H, I, J, K](
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
)(using ClassTag[X])
    extends MultiArrayT[X]:

  def flat(): Seq[X] = arr.toList
  def nested(): Seq[Seq[Seq[Seq[Seq[Seq[Seq[Seq[Seq[Seq[Seq[X]]]]]]]]]]] =
    augment.seq(this(_, _, _, _, _, _, _, _, _, _, _))(as, bs, cs, ds, es, fs, gs, hs, is, js, ks)
  def nestedAsJava(): JList[JList[JList[JList[JList[JList[JList[JList[JList[JList[JList[X]]]]]]]]]]] =
    nested()
      .map(
        _.map(
          _.map(
            _.map(_.map(_.map(_.map(_.map(_.map(_.map(_.asJava).asJava).asJava).asJava).asJava).asJava).asJava).asJava
          ).asJava
        ).asJava
      )
      .asJava

  def axes = List(as, bs, cs, ds, es, fs, gs, hs, is, js, ks)
  val length = axes.map(_.length).product
  val axisLengths = getAxisLengths(axes)
  def head = apply(as.head, bs.head, cs.head, ds.head, es.head, fs.head, gs.head, hs.head, is.head, js.head, ks.head)
  def isEmpty = axes.exists(_.isEmpty)
  def plot(title: String, addTimeStamp: Boolean, name: String, visibleAxes: Boolean) = ???

  def apply(a: A, b: B, c: C, d: D, e: E, f: F, g: G, h: H, i: I, j: J, k: K) =
    val vs = List(a, b, c, d, e, f, g, h, i, j, k)
    arr(getIndex(axes.lazyZip(vs).map(_ indexOf _), axisLengths, length, vs))

  def subArray[Z](a: A, b: B, c: C, d: D, e: E, f0: F, g: G, h: H, i: I, f: X => Z)(using
      ClassTag[Z]
  ): MultiArrayB[Z, J, K] =
    val vs = List(a, b, c, d, e, f0, g, h, i)
    val subIndexes = axes.lazyZip(vs).map(_ indexOf _)
    val ind = getIndex(subIndexes, axisLengths, length, vs)
    val sa = arr.slice(ind, ind + axisLengths(vs.size - 1))
    MultiArrayB(js, ks, sa.map(f).toArray)

  def subArray[Z](a: A, b: B, c: C, d: D, e: E, f0: F, g: G, f: X => Z)(using
      ClassTag[Z]
  ): MultiArrayD[Z, H, I, J, K] =
    val vs = List(a, b, c, d, e, f0, g)
    val subIndexes = axes.lazyZip(vs).map(_ indexOf _)
    val ind = getIndex(subIndexes, axisLengths, length, vs)
    val sa = arr.slice(ind, ind + axisLengths(vs.size - 1))
    MultiArrayD(hs, is, js, ks, sa.map(f).toArray)

  def getSubArraysB[Z](): MultiArrayI[MultiArrayB[X, J, K], A, B, C, D, E, F, G, H, I] =
    val seqs = nested().map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(x => multiArray(js, ks, x))))))))))
    val a = MultiArrayI(
      as,
      bs,
      cs,
      ds,
      es,
      fs,
      gs,
      hs,
      is,
      seqs.flatten.flatten.flatten.flatten.flatten.flatten.flatten.flatten.toArray
    )
    a

  def blockArrayC(): MultiArrayC[MultiArrayH[X, D, E, F, G, H, I, J, K], A, B, C] =
    val arr = nested().map(_.map(_.map(multiArray.apply(ds, es, fs, gs, hs, is, js, ks, _))))
    multiArray.apply(as, bs, cs, arr)

  def blockArrayG(): MultiArrayG[MultiArrayD[X, H, I, J, K], A, B, C, D, E, F, G] =
    val arr = nested().map(_.map(_.map(_.map(_.map(_.map(_.map(multiArray.apply(hs, is, js, ks, _))))))))
    multiArray.apply(as, bs, cs, ds, es, fs, gs, arr)

  def transform[Z](f: X => Z)(using ClassTag[Z]) =
    MultiArrayK(as, bs, cs, ds, es, fs, gs, hs, is, js, ks, arr.map(f))

  def transform[Z](f: (X, A, B, C, D, E, F, G, H, I, J, K) => Z)(using ClassTag[Z]) =
    val r0 =
      (nested() zip as).map((l1, a) =>
        (l1 zip bs).map((l2, b) =>
          (l2 zip cs).map((l3, c) =>
            (l3 zip ds).map((l4, d) =>
              (l4 zip es).map((l5, e) =>
                (l5 zip fs).map((l6, f0) =>
                  (l6 zip gs).map((l7, g) =>
                    (l7 zip hs).map((l8, h) =>
                      (l8 zip is).map((l9, i) =>
                        (l9 zip js).map((l10, j) => (l10 zip ks).map((x, k) => f(x, a, b, c, d, e, f0, g, h, i, j, k)))
                      )
                    )
                  )
                )
              )
            )
          )
        )
      )
    MultiArrayK(
      as,
      bs,
      cs,
      ds,
      es,
      fs,
      gs,
      hs,
      is,
      js,
      ks,
      r0.flatten.flatten.flatten.flatten.flatten.flatten.flatten.flatten.flatten.flatten.toArray
    )

  def saveToFile(
      folder: String,
      filePrefix: String,
      fileSuffix: String,
      hasMultiLineEntry: Boolean = false,
      entrySeparator: String = entryDividingLine,
      entryToSt: X => String = _.toString
  ) =
    val s0 = s"${axes.map(_.mkString("|")).mkString("\n")}\n$entryDividingLine\n"
    val separator = if hasMultiLineEntry then entrySeparator + "\n" else ""
    val s = arr.map(entryToSt).mkString(s"\n$separator")
    util.saveToFile(folder, filePrefix + fileSuffix, s"$s0$s\n", "array")

    def toSt[A](a: A) =
      a match
        case f: Float => floatToSt(f)
        case _        => a.toString

    def f(a: A, b: B, c: C, d: D, e: E, f: F, g: G, h: H, i: I) =
      val sa = subArray(a, b, c, d, e, f, g, h, i, x => x)
      val fileNm =
        s"${filePrefix}_${toSt(a)}_${toSt(b)}_${toSt(c)}_${toSt(d)}_${toSt(e)}_${toSt(f)}_${toSt(g)}_${toSt(h)}_${toSt(i)}.txt"
      sa.saveToFile(folder, fileNm, hasMultiLineEntry, entrySeparator, entryToSt)
    augment(f)(as, bs, cs, ds, es, fs, gs, hs, is)

  def toStr(xToStr: X => String = _.toString): String =
    (nested() zip as).map((x1, a) => s"  $a\n" + 
    (x1 zip bs).map((x2, b) => s"    $b\n" + 
    (x2 zip cs).map((x3, c) => s"      $c\n" + 
    (x3 zip ds).map((x4, d) => s"        $d\n" + 
    (x4 zip es).map((x5, e) => s"          $e\n" + 
    (x5 zip fs).map((x6, f) => s"            $f\n" + 
    (x6 zip gs).map((x7, g) => s"              $g\n" + 
    (x7 zip hs).map((x8, h) => s"                $h\n" + 
    (x8 zip is).map((x9, i) => s"                  $i\n" + 
    (x9 zip js).map((x10, j) => s"                    $j\n" + 
    (x10 zip ks).map((x11, k)  => s"$k ${xToStr (x11)}  ").mkString("\n          ")
    ).mkString("\n")).mkString("\n")).mkString("\n")).mkString("\n")).mkString("\n")).mkString("\n")).mkString("\n"))
    .mkString("\n")).mkString("\n")).mkString("\n")
