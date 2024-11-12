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
)(using ClassTag[X]):

  def flat(): Seq[X] = arr.toList
  def nested(): Seq[Seq[Seq[Seq[Seq[Seq[X]]]]]] = augment.seq(this(_, _, _, _, _, _))(as, bs, cs, ds, es, fs)
  def origAsJava(): JList[JList[JList[JList[JList[JList[X]]]]]] =
    nested().map(_.map(_.map(_.map(_.map(_.asJava).asJava).asJava).asJava).asJava).asJava
  
  def axes = List(as, bs, cs, ds, es, fs)
  val length = axes.map(_.length).product
  val axisLengths = getAxisLengths(axes)
  def head = apply(as.head, bs.head, cs.head, ds.head, es.head, fs.head)
  def isEmpty = as.isEmpty || bs.isEmpty || cs.isEmpty || ds.isEmpty || es.isEmpty || fs.isEmpty

  def apply(a: A, b: B, c: C, d: D, e: E, f: F) =
    val ia = as.indexOf(a)
    val ib = bs.indexOf(b)
    val ic = cs.indexOf(c)
    val id = ds.indexOf(d)
    val ie = es.indexOf(e)
    val if0 = fs.indexOf(f)
    val i = getIndex(Array(ia, ib, ic, id, ie, if0), axisLengths, length, List(a, b, c, d, e, f))
    arr(i)

  def subArray[Z](a: A, b: B, c: C, d: D, f: X => Z)(using ClassTag[Z]): MultiArrayB[Z, E, F] =
    val ia = as.indexOf(a)
    val ib = bs.indexOf(b)
    val ic = cs.indexOf(c)
    val id = ds.indexOf(d)
    val subIndexes = Array(ia, ib, ic, id, 0, 0)
    val ind = getIndex(subIndexes, axisLengths, length, List(a, b, c, d))
    val sa = arr.slice(ind, ind + axisLengths(3))
    MultiArrayB(es, fs, sa.map(f))

  def subArray[Z](a: A, b: B, c: C, f: X => Z)(using ClassTag[Z]): MultiArrayC[Z, D, E, F] =
    val ia = as.indexOf(a)
    val ib = bs.indexOf(b)
    val ic = cs.indexOf(c)
    val subIndexes = Array(ia, ib, ic, 0, 0, 0)
    val ind = getIndex(subIndexes, axisLengths, length, List(a, b, c))
    val sa = arr.slice(ind, ind + axisLengths(2))
    MultiArrayC(ds, es, fs, sa.map(f))

  def subArray[Z](a: A, b: B, f: X => Z)(using ClassTag[Z]): MultiArrayD[Z, C, D, E, F] =
    val ia = as.indexOf(a)
    val ib = bs.indexOf(b)
    val subIndexes = Array(ia, ib, 0, 0, 0, 0)
    val ind = getIndex(subIndexes, axisLengths, length, List(a, b))
    val sa = arr.slice(ind, ind + axisLengths(1))
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

  def getEFpoints[Z](e: E, f0: F): MultiArrayD[X, A, B, C, D] =
    getSubArraysB().transform(a => a.get(e, f0).get)

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
)(using ClassTag[X]):

  def flat(): Seq[X] = arr.toList
  def nested(): Seq[Seq[Seq[Seq[Seq[Seq[Seq[X]]]]]]] =
    augment.seq(this(_, _, _, _, _, _, _))(as, bs, cs, ds, es, fs, gs)
  def origAsJava(): JList[JList[JList[JList[JList[JList[JList[X]]]]]]] =
    nested().map(_.map(_.map(_.map(_.map(_.map(_.asJava).asJava).asJava).asJava).asJava).asJava).asJava

  def axes = List(as, bs, cs, ds, es, fs, gs)
  val length = axes.map(_.length).product
  val axisLengths = getAxisLengths(axes)
  def head = apply(as.head, bs.head, cs.head, ds.head, es.head, fs.head, gs.head)
  def isEmpty = as.isEmpty || bs.isEmpty || cs.isEmpty || ds.isEmpty || es.isEmpty || fs.isEmpty || gs.isEmpty

  def apply(a: A, b: B, c: C, d: D, e: E, f: F, g: G) =
    val ia = as.indexOf(a)
    val ib = bs.indexOf(b)
    val ic = cs.indexOf(c)
    val id = ds.indexOf(d)
    val ie = es.indexOf(e)
    val if0 = fs.indexOf(f)
    val ig = gs.indexOf(g)
    val i = getIndex(Array(ia, ib, ic, id, ie, if0, ig), axisLengths, length, List(a, b, c, d, e, f, g))
    arr(i)

  def checkSubIndexes(indexes: Array[Int]) =
    if indexes.exists(_ < 0) then throw Exception(s"Index not found:\n${this}")

  def checkSubIndexes(indexes: Array[Int], coords: Seq[Any]) =
    if indexes.exists(_ < 0) then
      throw Exception(s"Index not found for ${coords.mkString(" ")}:\n${this}\n${coords.mkString(" ")}")

  def subArray[Z](a: A, b: B, c: C, d: D, e: E, f: X => Z)(using ClassTag[Z]): MultiArrayB[Z, F, G] =
    val ia = as.indexOf(a)
    val ib = bs.indexOf(b)
    val ic = cs.indexOf(c)
    val id = ds.indexOf(d)
    val ie = es.indexOf(e)
    val subIndexes = Array(ia, ib, ic, id, ie, 0, 0)
    checkSubIndexes(subIndexes, List(a, b, c, d, e))
    val ind = getIndex(subIndexes, axisLengths, length, List(a, b, c, d, e))
    val sa = arr.slice(ind, ind + fs.length * gs.length)
    MultiArrayB(fs, gs, sa.map(f))

  def subArray[Z](a: A, b: B, c: C, f: X => Z)(using ClassTag[Z]): MultiArrayD[Z, D, E, F, G] =
    val ia = as.indexOf(a)
    val ib = bs.indexOf(b)
    val ic = cs.indexOf(c)
    val subIndexes = Array(ia, ib, ic, 0, 0, 0, 0)
    checkSubIndexes(subIndexes, List(a, b, c))
    val ind = getIndex(subIndexes, axisLengths, length, List(a, b, c))
    val sa = arr.slice(ind, ind + axisLengths(2))
    MultiArrayD(ds, es, fs, gs, sa.map(f))

  def getSubArraysB[Z](): MultiArrayE[MultiArrayB[X, F, G], A, B, C, D, E] =
    val seqs = nested().map(_.map(_.map(_.map(_.map(x => multiArray(fs, gs, x))))))
    MultiArrayE(as, bs, cs, ds, es, seqs.flatten.flatten.flatten.flatten.toArray)

  def getSubArraysEFG[Z](): MultiArrayD[MultiArrayC[X, E, F, G], A, B, C, D] =
    val seqs = nested().map(_.map(_.map(_.map(x => multiArray(es, fs, gs, x)))))
    MultiArrayD(as, bs, cs, ds, seqs.flatten.flatten.flatten.toArray)

  def getEFavgs[Z](using Numeric[X]): MultiArrayD[Float, A, B, C, D] =
    getSubArraysEFG().transform(a => a.arr.sum.toFloat / a.arr.length)

  def getEFGpoints[Z](e: E, f0: F, g: G): MultiArrayD[X, A, B, C, D] =
    getSubArraysEFG().transform(_.get(e, f0, g).get)

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
