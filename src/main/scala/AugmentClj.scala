package augmentedClj

/** Clojure-specific augmented functions: these are not Scala objects, but instead implement IFn and can be called
  * directly from Clojure
  */

import augmented._
import augmented.given
import basicdef.JList
import comprehension._
import comprehension.given
import mappable.{JFuture, MappableT, MappableW}
import multiarray._
import shape._

import collection.JavaConverters._

case class AugmentedFunctionCljB[Z, A, B, R[_, _, _], S[_, _, _]](f: (A, B) => Z)(using
    cx: ComprehensionB[R],
    cy: ComprehensionB[S]
) extends AugmentedFnB[Z, A, B, R, S]
    with AugmentedFnCljB[Z, A, B, R, S]

case class AugmentedFunctionCljC[Z, A, B, C, R[_, _, _, _], S[_, _, _, _]](f: (A, B, C) => Z)(using
    cx: ComprehensionC[R],
    cy: ComprehensionC[S]
) extends AugmentedFnC[Z, A, B, C, R, S]
    with AugmentedFnCljC[Z, A, B, C, R, S]

case class AugmentCljB[R[_, _, _], S[_, _, _]]()(using cx: ComprehensionB[R], cy: ComprehensionB[S]):
  def apply[Z, A, B](f: (A, B) => Z) = AugmentedFunctionCljB[Z, A, B, R, S](f)

case class AugmentCljC[R[_, _, _, _], S[_, _, _, _]]()(using cx: ComprehensionC[R], cy: ComprehensionC[S]):
  def apply[Z, A, B, C](f: (A, B, C) => Z) = AugmentedFunctionCljC[Z, A, B, C, R, S](f)

trait ClojureFn[V](f: clojure.lang.IFn) extends java.util.concurrent.Callable[V] with clojure.lang.IFn:
  def invoke(): Object = f.invoke()
  def invoke(arg1: Object) = f.invoke(arg1)
  def invoke(arg1: Object, arg2: Object) =
    f.invoke(arg1, arg2)
  def invoke(arg1: Object, arg2: Object, arg3: Object) =
    f.invoke(arg1, arg2, arg3)
  def invoke(arg1: Object, arg2: Object, arg3: Object, arg4: Object) =
    f.invoke(arg1, arg2, arg3, arg4)
  def invoke(arg1: Object, arg2: Object, arg3: Object, arg4: Object, arg5: Object) =
    f.invoke(arg1, arg2, arg3, arg4)
  def invoke(arg1: Object, arg2: Object, arg3: Object, arg4: Object, arg5: Object, arg6: Object) =
    f.invoke(arg1, arg2, arg3, arg4, arg5)
  def invoke(arg1: Object, arg2: Object, arg3: Object, arg4: Object, arg5: Object, arg6: Object, arg7: Object) =
    f.invoke(arg1, arg2, arg3, arg4, arg5, arg6)
  def invoke(
      arg1: Object,
      arg2: Object,
      arg3: Object,
      arg4: Object,
      arg5: Object,
      arg6: Object,
      arg7: Object,
      arg8: Object
  ) =
    f.invoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7)
  def invoke(
      arg1: Object,
      arg2: Object,
      arg3: Object,
      arg4: Object,
      arg5: Object,
      arg6: Object,
      arg7: Object,
      arg8: Object,
      arg9: Object
  ) =
    f.invoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8)
  def invoke(
      arg1: Object,
      arg2: Object,
      arg3: Object,
      arg4: Object,
      arg5: Object,
      arg6: Object,
      arg7: Object,
      arg8: Object,
      arg9: Object,
      arg10: Object
  ) =
    f.invoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9)
  def invoke(
      arg1: Object,
      arg2: Object,
      arg3: Object,
      arg4: Object,
      arg5: Object,
      arg6: Object,
      arg7: Object,
      arg8: Object,
      arg9: Object,
      arg10: Object,
      arg11: Object
  ) =
    f.invoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10)
  def invoke(
      arg1: Object,
      arg2: Object,
      arg3: Object,
      arg4: Object,
      arg5: Object,
      arg6: Object,
      arg7: Object,
      arg8: Object,
      arg9: Object,
      arg10: Object,
      arg11: Object,
      arg12: Object
  ) =
    f.invoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11)
  def invoke(
      arg1: Object,
      arg2: Object,
      arg3: Object,
      arg4: Object,
      arg5: Object,
      arg6: Object,
      arg7: Object,
      arg8: Object,
      arg9: Object,
      arg10: Object,
      arg11: Object,
      arg12: Object,
      arg13: Object
  ) =
    f.invoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12)
  def invoke(
      arg1: Object,
      arg2: Object,
      arg3: Object,
      arg4: Object,
      arg5: Object,
      arg6: Object,
      arg7: Object,
      arg8: Object,
      arg9: Object,
      arg10: Object,
      arg11: Object,
      arg12: Object,
      arg13: Object,
      arg14: Object
  ) =
    f.invoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13)
  def invoke(
      arg1: Object,
      arg2: Object,
      arg3: Object,
      arg4: Object,
      arg5: Object,
      arg6: Object,
      arg7: Object,
      arg8: Object,
      arg9: Object,
      arg10: Object,
      arg11: Object,
      arg12: Object,
      arg13: Object,
      arg14: Object,
      arg15: Object
  ) =
    f.invoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14)
  def invoke(
      arg1: Object,
      arg2: Object,
      arg3: Object,
      arg4: Object,
      arg5: Object,
      arg6: Object,
      arg7: Object,
      arg8: Object,
      arg9: Object,
      arg10: Object,
      arg11: Object,
      arg12: Object,
      arg13: Object,
      arg14: Object,
      arg15: Object,
      arg16: Object
  ) =
    f.invoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15)
  def invoke(
      arg1: Object,
      arg2: Object,
      arg3: Object,
      arg4: Object,
      arg5: Object,
      arg6: Object,
      arg7: Object,
      arg8: Object,
      arg9: Object,
      arg10: Object,
      arg11: Object,
      arg12: Object,
      arg13: Object,
      arg14: Object,
      arg15: Object,
      arg16: Object,
      arg17: Object
  ) =
    f.invoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15, arg16)
  def invoke(
      arg1: Object,
      arg2: Object,
      arg3: Object,
      arg4: Object,
      arg5: Object,
      arg6: Object,
      arg7: Object,
      arg8: Object,
      arg9: Object,
      arg10: Object,
      arg11: Object,
      arg12: Object,
      arg13: Object,
      arg14: Object,
      arg15: Object,
      arg16: Object,
      arg17: Object,
      arg18: Object
  ) =
    f.invoke(
      arg1,
      arg2,
      arg3,
      arg4,
      arg5,
      arg6,
      arg7,
      arg8,
      arg9,
      arg10,
      arg11,
      arg12,
      arg13,
      arg14,
      arg15,
      arg16,
      arg17
    )
  def invoke(
      arg1: Object,
      arg2: Object,
      arg3: Object,
      arg4: Object,
      arg5: Object,
      arg6: Object,
      arg7: Object,
      arg8: Object,
      arg9: Object,
      arg10: Object,
      arg11: Object,
      arg12: Object,
      arg13: Object,
      arg14: Object,
      arg15: Object,
      arg16: Object,
      arg17: Object,
      arg18: Object,
      arg19: Object
  ) =
    f.invoke(
      arg1,
      arg2,
      arg3,
      arg4,
      arg5,
      arg6,
      arg7,
      arg8,
      arg9,
      arg10,
      arg11,
      arg12,
      arg13,
      arg14,
      arg15,
      arg16,
      arg17,
      arg18
    )
  def invoke(
      arg1: Object,
      arg2: Object,
      arg3: Object,
      arg4: Object,
      arg5: Object,
      arg6: Object,
      arg7: Object,
      arg8: Object,
      arg9: Object,
      arg10: Object,
      arg11: Object,
      arg12: Object,
      arg13: Object,
      arg14: Object,
      arg15: Object,
      arg16: Object,
      arg17: Object,
      arg18: Object,
      arg19: Object,
      arg20: Object
  ) =
    f.invoke(
      arg1,
      arg2,
      arg3,
      arg4,
      arg5,
      arg6,
      arg7,
      arg8,
      arg9,
      arg10,
      arg11,
      arg12,
      arg13,
      arg14,
      arg15,
      arg16,
      arg17,
      arg18,
      arg19
    )

  def invoke(
      arg1: Object,
      arg2: Object,
      arg3: Object,
      arg4: Object,
      arg5: Object,
      arg6: Object,
      arg7: Object,
      arg8: Object,
      arg9: Object,
      arg10: Object,
      arg11: Object,
      arg12: Object,
      arg13: Object,
      arg14: Object,
      arg15: Object,
      arg16: Object,
      arg17: Object,
      arg18: Object,
      arg19: Object,
      arg20: Object,
      args: Object*
  ) =
    f.invoke(
      arg1,
      arg2,
      arg3,
      arg4,
      arg5,
      arg6,
      arg7,
      arg8,
      arg9,
      arg10,
      arg11,
      arg12,
      arg13,
      arg14,
      arg15,
      arg16,
      arg17,
      arg18,
      arg19,
      arg20,
      args
    )

  def applyTo(arglist: clojure.lang.ISeq) = f.applyTo(arglist)
  def call(): V = f.call().asInstanceOf[V]
  def run(): Unit = f.run()

case class ClojureFnA[X, Z, A, R[_, _], S[_, _]](f: clojure.lang.IFn, g: AugmentedFunctionA[Z, A, R, S])
    extends ClojureFn[X](f):
  override def invoke(arg1: Object) =
    arg1 match
      case (as: JList[_]) =>
        g(as.asInstanceOf[JList[A]])
      case _ =>
        f.invoke(arg1)

case class ClojureFnB[Y, Z, A, B, C, D, E, R[_, _, _], S[_, _, _]](
    f: clojure.lang.IFn,
    g: AugmentedFunctionCljB[Z, A, B, R, S]
) extends ClojureFn[Y](f):
  override def invoke(arg1: Object, arg2: Object) =
    (arg1, arg2) match
      case (as: JList[_], bs: JList[_]) =>
        g(as.asInstanceOf[JList[A]], bs.asInstanceOf[JList[B]])
      case (as: MappableT[_], b: Object) =>
        g(as.asInstanceOf[MappableT[A]], b.asInstanceOf[B])
      case (a: Object, bs: MappableT[_]) =>
        g(a.asInstanceOf[A], bs.asInstanceOf[MappableT[B]])
      case _ =>
        f.invoke(arg1, arg2)

case class ClojureFnC[Y, Z, A, B, C, D, E, R[_, _, _, _], S[_, _, _, _]](
    f: clojure.lang.IFn,
    g: AugmentedFunctionCljC[Z, A, B, C, R, S]
) extends ClojureFn[Y](f):
  override def invoke(arg1: Object, arg2: Object, arg3: Object) =
    (arg1, arg2, arg3) match
      case (as: JList[_], bs: JList[_], cs: JList[_]) =>
        g(as.asInstanceOf[JList[A]], bs.asInstanceOf[JList[B]], cs.asInstanceOf[JList[C]])

      case (as: MappableT[_], b: Object, c: Object) =>
        g(as.asInstanceOf[MappableT[A]], b.asInstanceOf[B], c.asInstanceOf[C])

      case (a: Object, bs: MappableT[_], c: Object) =>
        g(a.asInstanceOf[A], bs.asInstanceOf[MappableT[B]], c.asInstanceOf[C])

      case (a: Object, b: Object, cs: MappableT[_]) =>
        g(a.asInstanceOf[A], b.asInstanceOf[B], cs.asInstanceOf[MappableT[C]])

      case _ =>
        f.invoke(arg1, arg2, arg3)

  override def invoke(arg1: Object, arg2: Object, arg3: Object, arg4: Object) =
    (arg1, arg2, arg3, arg4) match
      case (
            as: JList[_],
            bsClj: clojure.lang.IFn,
            csClj: clojure.lang.IFn,
            phiClj: clojure.lang.IFn,
          ) =>
        g(as.asInstanceOf[JList[A]], bsClj, csClj, phiClj)
      case _ =>
        f.invoke(arg1, arg2, arg3, arg4)

case class ClojureFnD[Y, Z, A, B, C, D, E, R[_, _, _, _, _], S[_, _, _, _, _]](
    f: clojure.lang.IFn,
    g: AugmentedFunctionD[Z, A, B, C, D, R, S]
) extends ClojureFn[Y](f):
  override def invoke(arg1: Object, arg2: Object, arg3: Object, arg4: Object) =
    (arg1, arg2, arg3, arg4) match
      case (as: JList[_], bs: JList[_], cs: JList[_], ds: JList[_]) =>
        g(
          as.asInstanceOf[JList[A]],
          bs.asInstanceOf[JList[B]],
          cs.asInstanceOf[JList[C]],
          ds.asInstanceOf[JList[D]]
        )
      case _ =>
        f.invoke(arg1, arg2, arg3, arg4)

case class ClojureFnE[Y, Z, A, B, C, D, E, R[_, _, _, _, _, _], S[_, _, _, _, _, _]](
    f: clojure.lang.IFn,
    g: AugmentedFunctionE[Z, A, B, C, D, E, R, S]
) extends ClojureFn[Y](f):
  override def invoke(arg1: Object, arg2: Object, arg3: Object, arg4: Object, arg5: Object) =
    (arg1, arg2, arg3, arg4, arg5) match
      case (as: JList[_], bs: JList[_], cs: JList[_], ds: JList[_], es: JList[_]) =>
        g(
          as.asInstanceOf[JList[A]],
          bs.asInstanceOf[JList[B]],
          cs.asInstanceOf[JList[C]],
          ds.asInstanceOf[JList[D]],
          es.asInstanceOf[JList[E]]
        )
      case _ =>
        f.invoke(arg1, arg2, arg3, arg4, arg5)

object augment:

  def apply(f: clojure.lang.IFn): clojure.lang.IFn =
    val argCts = f.getClass.getDeclaredMethods.map(_.getParameterCount)
    val clojureFn = argCts.head match
      case 1 =>
        val g = (a: Object) => f.invoke(a)
        val h = AugmentA[MultiArrayA, SeqA]()(g)
        ClojureFnA(f, h)
      case 2 =>
        val g = (a: Object, b: Object) => f.invoke(a, b)
        val h = AugmentCljB[MultiArrayB, SeqB]()(g)
        ClojureFnB(f, h)
      case 3 =>
        val g = (a: Object, b: Object, c: Object) => f.invoke(a, b, c)
        val h = AugmentCljC[MultiArrayC, SeqC]()(g)
        ClojureFnC(f, h)
      case 4 =>
        val g = (a: Object, b: Object, c: Object, d: Object) => f.invoke(a, b, c, d)
        val h = AugmentD[MultiArrayD, SeqD]()(g)
        ClojureFnD(f, h)
      case 5 =>
        val g = (a: Object, b: Object, c: Object, d: Object, e: Object) => f.invoke(a, b, c, d, e)
        val h = AugmentE[MultiArrayE, SeqE]()(g)
        ClojureFnE(f, h)
      case _ =>
        throw Exception(s"Function not extended: ${f.getClass.getDeclaredMethods}")
    clojureFn

trait AugmentedFnCljB[Z, A, B, R[_, _, _], S[_, _, _]](using cx: ComprehensionB[R], cy: ComprehensionB[S])
    extends AugmentFnBBase[Z, A, B]:

  def apply(as: JList[A], bs: JList[B], phiClj: clojure.lang.IFn): clojure.lang.PersistentVector =
    val phi = (a: A, b: B) =>
      phiClj.invoke(a, b) match
        case res: java.lang.Boolean =>
          res
        case obj: Object =>
          throw Exception(s"Clojure function does not return a Boolean but instead: ${obj.getClass}")
    val v = AugmentB[SeqB, SeqB]()(as.asScala.toList, a => bs.asScala.toList.filter(phi(a, _)), f)
    clojure.lang.PersistentVector.create(v.irregComprehension[Z](id).toList.asJava)

trait AugmentedFnCljC[Z, A, B, C, R[_, _, _, _], S[_, _, _, _]](using cr: ComprehensionC[R], cs: ComprehensionC[S])
    extends AugmentedFnCBase[Z, A, B, C]:

  def apply(
      as: JList[A],
      bsClj: clojure.lang.IFn,
      csClj: clojure.lang.IFn,
      phiClj: clojure.lang.IFn
  ): clojure.lang.PersistentVector =

    val bsDep = (a: A) =>
      bsClj.invoke(a) match
        case res: clojure.lang.LongRange =>
          res.toArray().toList.asInstanceOf[Seq[B]]
        case obj: Object =>
          throw Exception(s"Clojure function does not return a sequence but instead: ${obj.getClass}")

    val csDep = (b: B) =>
      csClj.invoke(b) match
        case res: clojure.lang.LongRange =>
          res.toArray().toList.asInstanceOf[Seq[C]]
        case obj: Object =>
          throw Exception(s"Clojure function does not return a sequence but instead: ${obj.getClass}")

    val phi = (a: A, b: B, c: C) =>
      phiClj.invoke(a, b, c) match
        case res: java.lang.Boolean =>
          res
        case obj: Object =>
          throw Exception(s"Clojure function does not return a Boolean but instead: ${obj.getClass}")

    val v = AugmentC[SeqC, SeqC]()(
      as.asScala.toList,
      (a: A) => bsDep(a),
      (a, b) => csDep(b).filter(phi(a, b, _)),
      f
    )
    clojure.lang.PersistentVector.create(v.irregComprehension[Z](id).toList.asJava)

object Mapper:

  def mappable(ft: java.util.concurrent.Future[Object]): MappableW[Object, _] =
    MappableW(ft)
