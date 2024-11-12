package augmented

import basicdef._
import comprehension._
import comprehension.given
import mappable._
import mappable.given
import multiarray._
import shape._
import util._
import variant._

import collection.JavaConverters._
import scala.reflect.ClassTag

import java.util.Spliterators
import java.util.stream._

trait AugmentedFnA[Z, A, R[_, _], S[_, _]](using ComprehensionA[R], ComprehensionA[S]) extends AugmentedFnABase[Z, A]:

  val baseShape = AugmentA[R, S]()

  def apply(as: Seq[A])(using ClassTag[Z]): R[Z, A] =
    val v = baseShape(as, f)
    v.rectComprehension(id)

  def apply(it: Iterator[A])(using ClassTag[Z]): R[Z, A] =
    apply(it.toList)

  def apply(as: Seq[A], phi: A => Boolean): S[Z, A] =
    val v = baseShape(as, phi, f)
    v.irregComprehension(id)

  def apply[X](as: Seq[X], nextDS: (Seq[X], Seq[X]) => Seq[X]): Seq[Z] =
    def fr(s: Seq[X], l: Seq[X]): Seq[Z] =
      s match
        case Seq() =>
          if l.length == as.length then List(f(l.asInstanceOf[A])) else List()
        case _ =>
          s.flatMap: x =>
            val xl = List(x) ++ l
            fr(nextDS(as, xl), xl)
    fr(as, List())

  infix def applyMixedRect[T[_]](as: => Mixed[A, T])(using
      m: Mappable[T],
      ta: ClassTag[A],
      pa: Plain[A],
      pz: Plain[Z]
  ): T[Z] =
    applyRectangular(toDerived(as, m.isDelayed))

  infix def applyRectangular[T[_]: Mappable](as: => T[A]): T[Z] =
    val v = baseShape(as, f)
    v.rectComprehensionDerived[Z](id)

  def apply[T[_]: Mappable](as: => T[A]) =
    val v = baseShape(as, f)
    v.rectComprehensionDerived[Z](id)

  def apply(as: JList[A]): R[Z, A] =
    val as1 = as.asScala.toList
    given ClassTag[Z] = ClassTag(f(as1.head).getClass)
    apply(as1)

trait AugmentedFnTA[T[_]: Mappable, Z, A, R[_, _], S[_, _]](using ComprehensionA[R], ComprehensionA[S])
    extends AugmentedFnTABase[T, Z, A]:

  val baseShape = AugmentA[R, S]()

  def apply(as: T[A])(using ClassTag[Z]): T[Z] =
    val g = (a: A, z: Z) => z
    augment(g).applyStandardForm(as, f)

  def apply[U[_]: Mappable](as: U[A])(using ClassTag[Z]): U[T[Z]] =
    val v = baseShape(as, f)
    v.rectComprehensionDerived[T[Z]](id)

trait AugmentedFnApplicA[T[_]: Applicative, Z, A, R[_, _], S[_, _]](using ComprehensionA[R], ComprehensionA[S])
    extends AugmentedFnApplicABase[T, Z, A]:

  val tag = "Applicative A"
  val baseShape = AugmentA[R, S]()

  def apply(as: Seq[A])(using ClassTag[A]): T[List[Z]] =
    traversal(as)(f)

  def apply(as: => T[A]): T[Z] =
    for
      a <- as
      r <- f(a)
    yield r

trait AugmentedFnTUA[T[_]: Mappable, U[_]: Mappable, Z, A, R[_, _], S[_, _]](using ComprehensionA[R], ComprehensionA[S])
    extends AugmentedFnTUABase[T, U, Z, A]:

  val baseShape = AugmentA[R, S]()

trait AugmentedFnPredA[A, R[_, _], S[_, _]](using ComprehensionA[R], ComprehensionA[S]) extends AugmentedFnPredBase[A]:

  def apply(as: Seq[A])(using ClassTag[A]) =
    as.filter(f)

trait AugmentedFnB[Z, A, B, R[_, _, _], S[_, _, _]](using cx: ComprehensionB[R], cy: ComprehensionB[S])
    extends AugmentFnBBase[Z, A, B]:

  val baseShape = AugmentB[R, S]()

  def apply[X <: Product](x: X)(using mp: ProdB[X, A, B]) =
    val (a, b) = Tuple.fromProductTyped(x)
    f(a, b)

  def apply(as: Seq[A], bs: Seq[B])(using ClassTag[Z]): R[Z, A, B] =
    val v = baseShape(as, bs, f)
    v.rectComprehension(id)

  def apply(as: Seq[A], bsGen: GenSeqB[A, B]): S[Z, A, B] =
    val v = baseShape(as, a => seq(bsGen)(a), f)
    v.irregComprehension(id)

  def apply(as: Seq[A], bsGen: GenSeqB[A, B], phi: (A, B) => Boolean): S[Z, A, B] =
    val v = baseShape(as, a => seq(bsGen)(a).filter(phi(a, _)), f)
    v.irregComprehension(id)

  def equiv(as: Seq[A], bsGen: GenSeqB[A, B]) =
    for
      a <- as
      b <- seq(bsGen)(a)
    yield f(a, b)

  def apply(as: Seq[A], filter: A => Boolean, bs: DepSeqB[A, B]): S[Z, A, B] =
    val filteredSeq = as.filter(filter)
    apply(filteredSeq, bs)

  def apply(as: Seq[A], bf: (Seq[Z], A) => Seq[B]): Seq[Z] =
    as.foldLeft(List[Z]())((acc: Seq[Z], a: A) =>
      val bs = bf(acc, a)
      val zs = bs.map(b => f(a, b))
      (acc ++ zs).toSet.toList
    )

  def apply(tuples: Seq[(A, B)]): Seq[Z] = tuples.map(f(_, _))
  def apply(toTuples: () => LazyList[(A, B)]): LazyList[Z] = toTuples().map(f(_, _))
  def apply(tuples: LazyList[(A, B)]): LazyList[Z] = tuples.map(f(_, _))

  def apply(as: Set[A], bs: Set[B])(using ClassTag[Z]): Set[Z] =
    val v = VariantSetRectangularB(as, bs, f)
    v.rectComprehension[Z](id)

  def apply(as: Set[A], bsDep: DepSetB[A, B]): Set[Z] =
    val v = VariantSetIrregB(as, bsDep, f)
    v.irregComprehension[Z](id)

  def apply(as: Set[A], bs: Set[B], phi: (A, B) => Boolean): Set[Z] =
    apply(as, a => bs.filter(phi(a, _)))

  infix def applyRectangular[T[_]: Mappable](as: => T[A], bs: => T[B]): T[Z] =
    val v = baseShape(as, bs, f)
    v.rectComprehensionDerived[Z](id)

  /** This is equivalent to:
   *    for
   *      a <- as
   *      b <- bs(a)
   *    yield
   *      f(a, b)
   */
  infix def applyStandardForm[T[_]: Mappable](as: T[A], bs: DepTB[T, A, B]) =
    val v = VariantIrregDerivedB[T, Z, A, B, R, S](as, bs, f)
    v.irregComprehensionDerived[Z](id)

  infix def applyMixedRect[T[_]](as: => Mixed[A, T], bs: => Mixed[B, T])(using
      m: Mappable[T],
      ta: ClassTag[A],
      tb: ClassTag[B],
      pa: Plain[A],
      pb: Plain[B],
      pz: Plain[Z]
  ): T[Z] =
    applyRectangular(toDerived(as, m.isDelayed), toDerived(bs, m.isDelayed))

  infix def applyMixed[T[_]](
      as: => Mixed[A, T],
      bs: A => Mixed[B, T]
  )(using
      m: Mappable[T],
      ta: ClassTag[A],
      tb: ClassTag[B],
      pa: Plain[A],
      pb: Plain[B],
      pz: Plain[Z]
  ): T[Z] =
    applyStandardForm(toDerived(as, m.isDelayed), a => toDerived(bs(a), m.isDelayed))

  def apply[T[_]: Mappable](as: => Mixed[A, T], bs: => Mixed[B, T], z: Boolean = true)(using
      ta: ClassTag[A],
      tb: ClassTag[B],
      pa: Plain[A],
      pb: Plain[B],
      pz: Plain[Z]
  ): T[Z] =
    applyMixedRect(as, bs)

  def apply(as: MappableT[A], b: B) = as.getFnValueB1(f, b)
  def apply(a: A, bs: MappableT[B]) = bs.getFnValueB2(f, a)

  def apply(as: JList[A], bs: JList[B]): MultiArrayB[Z, A, B] =
    val (as1, bs1) = (as.asScala.toList, bs.asScala.toList)
    val v = AugmentB[MultiArrayB, SeqB]()(as1, bs1, f)
    given ClassTag[Z] = ClassTag(f(as1.head, bs1.head).getClass)
    v.rectComprehension[Z](id)

  def apply(as: JList[A], bs: JList[B], phi: (A, B) => Boolean): JList[Z] =
    val v = AugmentB[SeqB, SeqB]()(as.asScala.toList, a => bs.asScala.toList.filter(phi(a, _)), f)
    v.irregComprehension[Z](id).toList.asJava

  def apply(as: JList[A], bsDep: JDepSeq[A, B]): JList[Z] =
    val v = AugmentB[SeqB, SeqB]()(as.asScala.toList, bsDep(_).asScala.toList, f)
    v.irregComprehension[Z](id).toList.asJava

  def applySeq(as: JList[A], bsDep: JDepSeq[A, B]): JList[JList[Z]] =
    val v = AugmentB[SeqNB, SeqNB]()(as.asScala.toList, a => bsDep(a).asScala.toList, f)
    v.irregComprehension[Z](id).map(_.asJava).asJava

  def apply(as: JList[A], bsDep: JDepSeq[A, B], phi: (A, B) => Boolean): JList[Z] =
    val v = AugmentB[SeqB, SeqB]()(as.asScala.toList, a => bsDep(a).asScala.toList.filter(phi(a, _)), f)
    v.irregComprehension[Z](id).toList.asJava

  def applySeq(as: BaseStream[A, ?], bsDep: JDepStream[A, B]): JList[JList[Z]] =
    val v = AugmentB[SeqNB, SeqNB]()(streamToLazyList(as), a => streamToLazyList(bsDep(a)), f)
    v.irregComprehension[Z](id).map(_.asJava).asJava

  def apply(as: BaseStream[A, ?], bs: BaseStream[B, ?]): MultiArrayB[Z, A, B] =
    val (as1, bs1) = (streamToLazyList(as), streamToLazyList(bs))
    val v = AugmentB[MultiArrayB, SeqB]()(as1, bs1, f)
    given ClassTag[Z] = ClassTag(f(as1.head, bs1.head).getClass)
    v.rectComprehension[Z](id)

  def apply(as: JList[A], bs: BaseStream[B, ?]): MultiArrayB[Z, A, B] =
    val (as1, bs1) = (as.asScala.toList, streamToLazyList(bs))
    val v = AugmentB[MultiArrayB, SeqB]()(as1, bs1, f)
    given ClassTag[Z] = ClassTag(f(as1.head, bs1.head).getClass)
    v.rectComprehension[Z](id)

  def apply(as: BaseStream[A, ?], bs: JList[B], phi: (A, B) => Boolean): JList[Z] =
    val v = AugmentB[SeqB, SeqB]()(streamToLazyList(as), a => bs.asScala.filter(phi(a, _)).toList, f)
    v.irregComprehension[Z](id).asJava

  def apply(as: BaseStream[A, ?], bsDep: JDepStream[A, B]): BaseStream[Z, ?] =
    val v = AugmentB[SeqB, SeqB]()(streamToLazyList(as), a => streamToLazyList(bsDep(a)), f)
    val res = v.irregComprehension[Z](id)
    StreamSupport.stream(Spliterators.spliteratorUnknownSize(res.iterator.asJava, 0), false)

  infix def crossCheck(
      g: (Seq[A], Seq[B]) => Seq[Z],
      as: Seq[A],
      bs: Seq[B],
      showFull: Boolean = true,
      show: Boolean = false
  ) =
    val (r, t) = timed(g(as, bs))

    val vf = AugmentB[SeqB, SeqB]()(as, bs, f)
    val (rf, tf) = timed(vf.irregComprehension[Z](id))
    displayCheckRes(r, rf, t, tf, showFull, show)
    assert(r == rf)
    (r, rf)

  infix def crossCheck(as: Seq[A], bs: Seq[B], g: (Seq[A], Seq[B]) => Seq[Z]): (Seq[Z], Seq[Z]) =
    crossCheck(g, as, bs)

  infix def crossCheck(as: Seq[A], bs: DepSeqB[A, B], g: (Seq[A], DepSeqB[A, B]) => Seq[Z]): Unit =
    val (r, t) = timed(g(as, bs))
    val vf = AugmentB[SeqB, SeqB]()(as, bs, f)
    val (rf, tf) = timed(vf.irregComprehension[Z](id))
    displayCheckRes(r, rf, t, tf, false, false)
    (r, rf)

  infix def crossCheck(as: Seq[A], bs: GenSeqB[A, B], r: Seq[Z]) =
    val vf = AugmentB[SeqB, SeqB]()(as, bs, f)
    val (rf, tf) = timed(vf.irregComprehension[Z](id))
    displayCheckRes(r, rf, 0, tf, false, false)
    assert(r == rf)
    (r, rf)

  infix def crossCheck(as: Set[A], bs: Set[B], g: (Set[A], Set[B]) => Set[Z])(using ClassTag[Z]): Unit =
    crossCheck(g, as, bs)

  infix def crossCheck(g: (Set[A], Set[B]) => Set[Z], as: Set[A], bs: Set[B])(using ClassTag[Z]) =
    val (r, t) = timed(g(as, bs))
    val vf = VariantSetRectangularB[Z, A, B](as, bs, f)
    val (rf, tf) = timed(vf.rectComprehension[Z](id))
    displayCheckRes(r, rf, 0, tf, false, false)
    assert(r == rf)

  infix def crossCheckFX[T[_]: Mappable](as: => Mixed[A, T], bs: => Mixed[B, T], r: T[Z])(using
      ta: ClassTag[A],
      tb: ClassTag[B],
      pa: Plain[A],
      pb: Plain[B],
      pz: Plain[Z]
  ): (T[Z], T[Z]) =
    val (rf, tf) = timed(apply(as, bs))
    assert(r == rf)
    (r, rf)

def displayCheckRes[A](orig: A, augmented: A, origTm: Double, augmentedTm: Double, showFull: Boolean, show: Boolean) =
  if show then
    if showFull then
      println(f"\nOrig:\n\t${orig}" + (if origTm > 0 then f"\n\t[t: $origTm%7.0f]" else ""))
      println(f"Augmented:\n\t${augmented}" + (if origTm > 0 then f"\n\t[t: $augmentedTm%7.0f]\n" else ""))
    else
      println(f"\nOrig:\t[t: $origTm%7.0f]")
      println(f"Augmented:\t[t: $augmentedTm%7.0f]\n")

trait AugmentedFnApplicB[T[_]: Applicative, Z, A, B, R[_, _, _], S[_, _, _]](using ComprehensionB[R], ComprehensionB[S])
    extends AugmentedFnApplicBBase[T, Z, A, B]:

  val tag = "Applicative B"
  val baseShape = AugmentB[R, S]()

  def apply(as: => Mixed[A, T], bs: => Mixed[B, T])(using
      aug: AugmentB[R, S],
      ta: ClassTag[A],
      tb: ClassTag[B],
      tz: ClassTag[Z],
      pa: Plain[A],
      pb: Plain[B],
      pz: Plain[Z]
  ) =
    sequence(as, bs, f)

trait AugmentedFnSingleTypeB[A, R[_, _, _], S[_, _, _]](using cx: ComprehensionB[R], cy: ComprehensionB[S])
    extends AugmentFnSingleTypeBBase[A]:

  val tag = "Single type B"

  def apply(as: Seq[A]): A =
    as.drop(1).fold(as.head)(f)

  def apply[T[_]: Applicative](as: Seq[T[A]])(using ClassTag[A]): T[A] =
    val g = augment(f)
    as.drop(1).fold(as.head)((as1, as2) => g(as1, as2))

trait AugmentedFnC[Z, A, B, C, R[_, _, _, _], S[_, _, _, _]](using cx: ComprehensionC[R], cy: ComprehensionC[S])
    extends AugmentedFnCBase[Z, A, B, C]:

  val baseShape = AugmentC[R, S]()

  def apply[X <: Product](x: X)(using mp: ProdC[X, A, B, C]) =
    val (a, b, c) = Tuple.fromProductTyped(x)
    f(a, b, c)

  def apply(as: Seq[A], bs: Seq[B], cs: Seq[C])(using ClassTag[Z]): R[Z, A, B, C] =
    val v = baseShape(as, bs, cs, f)
    v.rectComprehension[Z](id)

  def apply(a: A, bs: Seq[B], cs: Seq[C])(using ClassTag[Z]): R[Z, A, B, C] = apply(List(a), bs, cs)
  def apply(as: Seq[A], b: B, cs: Seq[C])(using ClassTag[Z]): R[Z, A, B, C] = apply(as, List(b), cs)
  def apply(as: Seq[A], bs: Seq[B], c: C)(using ClassTag[Z]): R[Z, A, B, C] = apply(as, bs, List(c))
  def apply(as: Seq[A], b: B, c: C)(using ClassTag[Z]): R[Z, A, B, C] = apply(as, List(b), List(c))
  def apply(a: A, bs: Seq[B], c: C)(using ClassTag[Z]): R[Z, A, B, C] = apply(List(a), bs, List(c))
  def apply(a: A, b: B, cs: Seq[C])(using ClassTag[Z]): R[Z, A, B, C] = apply(List(a), List(b), cs)

  def apply(as: Seq[A], bs: GenSeqB[A, B], cs: GenSeqC[A, B, C]) =
    val v = baseShape(as, bs, cs, f)
    v.irregComprehension[Z](id)

  def apply(as: Seq[A], bs: DepSeqB[A, B], cs: DepSeqB[B, C]): S[Z, A, B, C] =
    apply(as, bs, cs, (a, b, c) => true)

  def apply(as: Seq[A], bs: DepSeqB[A, B], cs: DepSeqC[A, B, C]): S[Z, A, B, C] =
    val v = baseShape(as, bs, cs, f)
    v.irregComprehension[Z](id)

  def apply(as: Seq[A], bs: GenSeqB[A, B], cs: GenSeqB[B, C], phi: (A, B, C) => Boolean): S[Z, A, B, C] =
    val v = baseShape(as, bs, (a, b) => seq(cs)(b).filter(phi(a, b, _)), f)
    v.irregComprehension[Z](id)

  def applySet(as: Set[A], bs: DepSetB[A, B], cs: DepSetC[A, B, C]) =
    val v = VariantSetIrregC(as, bs, cs, f)
    v.irregComprehension[Z](id)

  infix def applyPlainForm[T[_]: Mappable](a: => A, bs: => DepB[A, B], cs: => DepC[A, B, C]) =
    applyStandardForm(a.unit(), bs(_).unit(), cs(_, _).unit())

  infix def applyRectangular[T[_]: Mappable](as: => T[A], bs: => T[B], cs: => T[C]) =
    val v = baseShape(as, bs, cs, f)
    v.rectComprehensionDerived[Z](id)

  /** This is equivalent to:
   *    for
   *      a <- as
   *      b <- bs(a)
   *      c <- cs(a, b)
   *    yield
   *      f(a, b, c)
   */
  infix def applyStandardForm[T[_]: Mappable](as: T[A], bs: DepTB[T, A, B], cs: DepTC[T, A, B, C]) =
    val v = VariantIrregDerivedC[T, Z, A, B, C, R, S](as, bs, cs, f)
    v.irregComprehensionDerived[Z](id)

  /** The "threaded" form is equivalent to:
   *    for
   *      a <- as
   *      b <- bs(a)
   *      c <- cs(b)
   *    yield
   *      f(a, b, c)
   */
  def applyThreaded[T[_]: Mappable](as: T[A], bs: DepTB[T, A, B], cs: DepTB[T, B, C]) =
    applyStandardForm(as, bs, (_, b) => cs(b))

  infix def applyMixedRect[T[_]](as: => Mixed[A, T], bs: => Mixed[B, T], cs: => Mixed[C, T])(using
      m: Mappable[T],
      ta: ClassTag[A],
      tb: ClassTag[B],
      tc: ClassTag[C],
      pa: Plain[A],
      pb: Plain[B],
      pc: Plain[C],
      pz: Plain[Z]
  ) =
    applyRectangular(toDerived(as, m.isDelayed), toDerived(bs, m.isDelayed), toDerived(cs, m.isDelayed))

  infix def applyMixed[T[_]](as: => Mixed[A, T], bs: A => Mixed[B, T], cs: (A, B) => Mixed[C, T])(using
      m: Mappable[T],
      ta: ClassTag[A],
      tb: ClassTag[B],
      tc: ClassTag[C],
      pa: Plain[A],
      pb: Plain[B],
      pc: Plain[C],
      pz: Plain[Z]
  ) =
    applyStandardForm(toDerived(as, m.isDelayed), a => toDerived(bs(a), m.isDelayed), (a, b) => toDerived(cs(a, b), m.isDelayed))

  def apply[T[_]: Mappable](as: => T[A], bs: => T[B], cs: => T[C]) =
    applyRectangular(as, bs, cs)

  def apply[T[_]: Mappable](as: => Mixed[A, T], bs: => Mixed[B, T], cs: => Mixed[C, T], b: Boolean = true)(using
      ta: ClassTag[A],
      tb: ClassTag[B],
      tc: ClassTag[C],
      pa: Plain[A],
      pb: Plain[B],
      pc: Plain[C],
      pz: Plain[Z]
  ) =
    applyMixedRect(as, bs, cs)

  def apply[T[_]: Mappable](as: T[A], bs: T[B], cs: DepTB[T, B, C]) =
    applyStandardForm(as, _ => bs, (_, b) => cs(b))

  def apply[T[_]: Mappable](as: T[A], bs: DepTB[T, A, B], cs: DepTC[T, A, B, C]) =
    applyStandardForm(as, bs, cs)

  def apply(as: MappableT[A], b: B, c: C) = as.getFnValueC1(f, b, c)
  def apply(a: A, bs: MappableT[B], c: C) = bs.getFnValueC2(f, a, c)
  def apply(a: A, b: B, cs: MappableT[C]) = cs.getFnValueC3(f, a, b)

  def apply(as: JList[A], bs: JList[B], cs: JList[C]): R[Z, A, B, C] =
    val (as1, bs1, cs1) = (as.asScala.toList, bs.asScala.toList, cs.asScala.toList)
    given ClassTag[Z] = ClassTag(f(as1.head, bs1.head, cs1.head).getClass)
    val v = baseShape(as1, bs1, cs1, f)
    v.rectComprehension(id)

  infix def applyJ(
      as: JList[A],
      bs: JDepSeq[A, B],
      cs: JDepSeq[B, C],
      phi: (A, B, C) => Boolean = (a, b, c) => true
  ): JList[Z] =
    val v = AugmentC[SeqC, SeqC]()(
      as.asScala.toList,
      ((a: A) => bs(a).asScala.toList),
      (a, b) => cs(b).asScala.toList.filter(phi(a, b, _)),
      f
    )
    v.irregComprehension[Z](id).toList.asJava

  def apply(as: JOption[A], bs: JOption[B], cs: JOption[C]): JOption[Z] =
    applyRectangular(as, bs, cs)

  def apply(a: A, bs: JOption[B], c: C): JOption[Z] =
    applyRectangular(java.util.Optional.of(a), bs, java.util.Optional.of(c))

  def apply(a: A, b: B, cs: JOption[C]): JOption[Z] =
    applyRectangular(java.util.Optional.of(a), java.util.Optional.of(b), cs)

  def apply(as: JOption[A], bs: JOption[B], c: C): JOption[Z] =
    applyRectangular(as, bs, java.util.Optional.of(c))

  def apply(as: BaseStream[A, ?], bs: BaseStream[B, ?], cs: JList[C]): R[Z, A, B, C] =
    apply(streamToList(as), streamToList(bs), cs)

  def apply(vs: Seq[(A, B, C)]): Seq[Z] = vs.map(f(_, _, _))

  infix def crossCheck(as: Seq[A], bs: GenSeqB[A, B], cs: GenSeqC[A, B, C], r: Seq[Z]) =
    val vf = AugmentC[SeqC, SeqC]()(as, bs, cs, f)
    val (rf, tf) = timed(vf.irregComprehension[Z](id))
    displayCheckRes(r, rf, 0, tf, false, false)
    assert(r == rf)
    (r, rf)

  infix def crossCheck(as: Seq[A], bs: Seq[B], cs: Seq[C], g: (Seq[A], Seq[B], Seq[C]) => Seq[Z]): (Seq[Z], Seq[Z]) =
    crossCheck(g, as, bs, cs)

  infix def crossCheck(g: (Seq[A], Seq[B], Seq[C]) => Seq[Z], as: Seq[A], bs: Seq[B], cs: Seq[C]) =
    val (r, t) = timed(g(as, bs, cs))
    val vf = AugmentC[SeqC, SeqC]()(as, bs, cs, f)
    val (rf, tf) = timed(vf.irregComprehension[Z](id))
    displayCheckRes(r, rf, t, tf, false, false)
    assert(r == rf)
    (r, rf)

  infix def crossCheckFX[T[_]](as: => Mixed[A, T], bs: => Mixed[B, T], cs: => Mixed[C, T], r: T[Z])(using
      m: Mappable[T],
      ta: ClassTag[A],
      tb: ClassTag[B],
      tc: ClassTag[C],
      pa: Plain[A],
      pb: Plain[B],
      pc: Plain[C],
      pz: Plain[Z]
  ): (T[Z], T[Z]) =
    val (rf, tf) = timed(apply(as, bs, cs))
    assert(r == rf)
    (r, rf)

trait AugmentedFnApplicC[T[_]: Applicative, Z, A, B, C, R[_, _, _, _], S[_, _, _, _]](using
    ComprehensionC[R],
    ComprehensionC[S]
) extends AugmentedFnApplicCBase[T, Z, A, B, C]:

  val tag = "Applicative C"
  val baseShape = AugmentC[R, S]()

  def apply(as: => Mixed[A, T], bs: => Mixed[B, T], cs: => Mixed[C, T])(using
      aug: AugmentC[R, S],
      ta: ClassTag[A],
      tb: ClassTag[B],
      tc: ClassTag[C],
      tz: ClassTag[Z],
      pa: Plain[A],
      pb: Plain[B],
      pc: Plain[C],
      pz: Plain[Z]
  ) =
    sequence(as, _ => bs, (_, _) => cs, f)

trait AugmentedFnD[Z, A, B, C, D, R[_, _, _, _, _], S[_, _, _, _, _]](using
    cx: ComprehensionD[R],
    cy: ComprehensionD[S]
) extends AugmentedFnDBase[Z, A, B, C, D]:

  val baseShape = AugmentD[R, S]()

  def apply[X <: Product](x: X)(using mp: ProdD[X, A, B, C, D]) =
    val (a, b, c, d) = Tuple.fromProductTyped(x)
    f(a, b, c, d)

  def apply(as: Seq[A], bs: Seq[B], cs: Seq[C], ds: Seq[D])(using ClassTag[Z]) =
    val v = baseShape(as, bs, cs, ds, f)
    v.rectComprehension(id)

  def apply(as: Seq[A], bs: GenSeqB[A, B], cs: GenSeqC[A, B, C], ds: GenSeqD[A, B, C, D]): S[Z, A, B, C, D] =
    val v = baseShape(as, seq(bs), seq(cs), seq(ds), f)
    v.irregComprehension[Z](id)

  def apply(
      as: Seq[A],
      bsGen: GenSeqB[A, B],
      csGen: GenSeqC[A, B, C],
      dsGen: GenSeqD[A, B, C, D],
      phi: (A, B, C, D) => Boolean
  )(using ClassTag[D]): S[Z, A, B, C, D] =
    val ds_ = (a: A, b: B, c: C) => seq(dsGen)(a, b, c).filter(phi(a, b, c, _))
    val v = baseShape(as, seq(bsGen), seq(csGen), ds_, f)
    v.irregComprehension[Z](id)

  def apply[E, F](es: Seq[E], fs: Seq[F], g: (E, F) => (A, B, C, D))(using ClassTag[Z]) =
    val v = AugmentB[MultiArrayB, SeqB]()(es, fs, g)
    v.rectComprehension(f(_, _, _, _))

  def applySet(as: Set[A], bs: DepSetB[A, B], cs: DepSetC[A, B, C], ds: DepSetD[A, B, C, D]) =
    val v = VariantSetIrregD(as, bs, cs, ds, f)
    v.irregComprehension[Z](id)

  infix def applyPlainForm[T[_]: Mappable](a: => A, bs: => DepB[A, B], cs: => DepC[A, B, C], ds: => DepD[A, B, C, D]) =
    applyStandardForm(a.unit(), bs(_).unit(), cs(_, _).unit(), ds(_, _, _).unit())

  infix def applyRectangular[T[_]: Mappable](as: T[A], bs: T[B], cs: T[C], ds: T[D]) =
    val v = baseShape(as, bs, cs, ds, f)
    v.rectComprehensionDerived[Z](id)

  /** This is equivalent to:
   *    for
   *      a <- as
   *      b <- bs(a)
   *      c <- cs(a, b)
   *      d <- ds(a, b, c)
   *    yield
   *      f(a, b, c, d)
   */
  infix def applyStandardForm[T[_]: Mappable](
      as: T[A],
      bs: DepTB[T, A, B],
      cs: DepTC[T, A, B, C],
      ds: DepTD[T, A, B, C, D]
  ) =
    val v = VariantIrregDerivedD[T, Z, A, B, C, D, R, S](as, bs, cs, ds, f)
    v.irregComprehensionDerived[Z](id)

  /** The "threaded" form is equivalent to:
   *    for
   *      a <- as
   *      b <- bs(a)
   *      c <- cs(b)
   *      d <- ds(c)
   *    yield
   *      f(a, b, c, d)
   */
  def applyThreaded[T[_]: Mappable](as: T[A], bs: DepTB[T, A, B], cs: DepTB[T, B, C], ds: DepTB[T, C, D]) =
    applyStandardForm(as, bs, (_, b) => cs(b), (_, _, c) => ds(c))

  infix def applyMixed[T[_]](
      as: => Mixed[A, T],
      bs: A => Mixed[B, T],
      cs: (A, B) => Mixed[C, T],
      ds: (A, B, C) => Mixed[D, T]
  )(using
      m: Mappable[T],
      ta: ClassTag[A],
      tb: ClassTag[B],
      tc: ClassTag[C],
      td: ClassTag[D],
      pa: Plain[A],
      pb: Plain[B],
      pc: Plain[C],
      pd: Plain[D],
      pz: Plain[Z]
  ): T[Z] =
    applyStandardForm(
      toDerived(as, m.isDelayed),
      a => toDerived(bs(a), m.isDelayed),
      (a, b) => toDerived(cs(a, b), m.isDelayed),
      (a, b, c) => toDerived(ds(a, b, c), m.isDelayed)
    )

  def apply(as: JList[A], bs: JList[B], cs: JList[C], ds: JList[D]): MultiArrayD[Z, A, B, C, D] =
    val (as1, bs1, cs1, ds1) = (as.asScala.toList, bs.asScala.toList, cs.asScala.toList, ds.asScala.toList)
    given ClassTag[Z] = ClassTag(f(as1.head, bs1.head, cs1.head, ds1.head).getClass)
    val v = AugmentD[MultiArrayD, SeqD]()(as1, bs1, cs1, ds1, f)
    v.rectComprehension(id)

  def apply(
      as: BaseStream[A, ?],
      bs: BaseStream[B, ?],
      cs: BaseStream[C, ?],
      ds: BaseStream[D, ?]
  ): MultiArrayD[Z, A, B, C, D] =
    val (as1, bs1, cs1, ds1) = (streamToLazyList(as), streamToLazyList(bs), streamToLazyList(cs), streamToLazyList(ds))
    val v = AugmentD[MultiArrayD, SeqD]()(as1, bs1, cs1, ds1, f)
    given ClassTag[Z] = ClassTag(f(as1.head, bs1.head, cs1.head, ds1.head).getClass)
    v.rectComprehension[Z](id)

trait AugmentedFnE[Z, A, B, C, D, E, R[_, _, _, _, _, _], S[_, _, _, _, _, _]](using
    cx: ComprehensionE[R],
    cy: ComprehensionE[S]
) extends AugmentedFnEBase[Z, A, B, C, D, E]:

  val baseShape = AugmentE[R, S]()

  def apply[X <: Product](x: X)(using mp: ProdE[X, A, B, C, D, E]) =
    val (a, b, c, d, e) = Tuple.fromProductTyped(x)
    f(a, b, c, d, e)

  def apply(as: Seq[A], bs: Seq[B], cs: Seq[C], ds: Seq[D], es: Seq[E])(using ClassTag[Z]): R[Z, A, B, C, D, E] =
    val v = baseShape(as, bs, cs, ds, es, f)
    v.rectComprehension(id)

  def apply(as: Seq[A], bs: GenSeqB[A, B], cs: GenSeqC[A, B, C], ds: GenSeqD[A, B, C, D], es: GenSeqE[A, B, C, D, E]) =
    val v = baseShape(as, seq(bs), seq(cs), seq(ds), seq(es), f)
    v.irregComprehension[Z](id)

  infix def applyPlainForm[T[_]: Mappable](
      a: => A,
      bs: DepB[A, B],
      cs: DepC[A, B, C],
      ds: DepD[A, B, C, D],
      es: DepE[A, B, C, D, E]
  ) =
    applyStandardForm(a.unit(), bs(_).unit(), cs(_, _).unit(), ds(_, _, _).unit(), es(_, _, _, _).unit())

  infix def applyRectangular[T[_]: Mappable](as: T[A], bs: T[B], cs: T[C], ds: T[D], es: T[E]) =
    val v = baseShape(as, bs, cs, ds, es, f)
    v.rectComprehensionDerived[Z](id)

  /** This is equivalent to:
   *    for
   *      a <- as
   *      b <- bs(a)
   *      c <- cs(a, b)
   *      d <- ds(a, b, c)
   *      e <- es(a, b, c, d) 
   *    yield
   *      f(a, b, c, d, e)
   */
  infix def applyStandardForm[T[_]: Mappable](
      as: T[A],
      bs: DepTB[T, A, B],
      cs: DepTC[T, A, B, C],
      ds: DepTD[T, A, B, C, D],
      es: DepTE[T, A, B, C, D, E]
  ) =
    val v = VariantIrregDerivedE[T, Z, A, B, C, D, E, R, S](as, bs, cs, ds, es, f)
    v.irregComprehensionDerived[Z](id)

  /** The "threaded" form is equivalent to:
   *    for
   *      a <- as
   *      b <- bs(a)
   *      c <- cs(b)
   *      d <- ds(c)
   *      e <- es(d)
   *    yield
   *      f(a, b, c, d, e)
   */
  def applyThreaded[T[_]: Mappable](
      as: T[A],
      bs: DepTB[T, A, B],
      cs: DepTB[T, B, C],
      ds: DepTB[T, C, D],
      es: DepTB[T, D, E]
  ) =
    applyStandardForm(as, bs, (_, b) => cs(b), (_, _, c) => ds(c), (_, _, _, d) => es(d))

  infix def applyMixed[T[_]](
      as: => Mixed[A, T],
      bs: A => Mixed[B, T],
      cs: (A, B) => Mixed[C, T],
      ds: (A, B, C) => Mixed[D, T],
      es: (A, B, C, D) => Mixed[E, T]
  )(using
      m: Mappable[T],
      ta: ClassTag[A],
      tb: ClassTag[B],
      tc: ClassTag[C],
      td: ClassTag[D],
      te: ClassTag[E],
      pa: Plain[A],
      pb: Plain[B],
      pc: Plain[C],
      pd: Plain[D],
      pe: Plain[E],
      pz: Plain[Z]
  ): T[Z] =
    applyStandardForm(
      toDerived(as, m.isDelayed),
      a => toDerived(bs(a), m.isDelayed),
      (a, b) => toDerived(cs(a, b), m.isDelayed),
      (a, b, c) => toDerived(ds(a, b, c), m.isDelayed),
      (a, b, c, d) => toDerived(es(a, b, c, d), m.isDelayed)
    )

  def apply[F, G](fs: Seq[F], gs: Seq[G], g: (F, G) => (A, B, C, D, E))(using ClassTag[Z]) =
    val v = AugmentB[MultiArrayB, SeqB]()(fs, gs, g)
    v.rectComprehension(f(_, _, _, _, _))

  def apply(as: JList[A], bs: JList[B], cs: JList[C], ds: JList[D], es: JList[E]) =
    val (as1, bs1, cs1, ds1, es1) =
      (as.asScala.toList, bs.asScala.toList, cs.asScala.toList, ds.asScala.toList, es.asScala.toList)
    given ClassTag[Z] = ClassTag(f(as1.head, bs1.head, cs1.head, ds1.head, es1.head).getClass)
    val v = baseShape(as1, bs1, cs1, ds1, es1, f)
    v.rectComprehension(id)

trait AugmentedFnF[Z, A, B, C, D, E, F, R[_, _, _, _, _, _, _], S[_, _, _, _, _, _, _]](using
    cx: ComprehensionF[R],
    cy: ComprehensionF[S]
) extends AugmentedFnFBase[Z, A, B, C, D, E, F]:

  val baseShape = AugmentF[R, S]()

  def apply[X <: Product](x: X)(using mp: ProdF[X, A, B, C, D, E, F]) =
    val (a, b, c, d, e, f0) = Tuple.fromProductTyped(x)
    f(a, b, c, d, e, f0)

  def apply(as: Seq[A], bs: Seq[B], cs: Seq[C], ds: Seq[D], es: Seq[E], fs: Seq[F])(using
      ClassTag[Z]
  ): R[Z, A, B, C, D, E, F] =
    val v = baseShape(as, bs, cs, ds, es, fs, f)
    v.rectComprehension(id)

  infix def applyPlainForm[T[_]: Mappable](
      a: => A,
      bs: DepB[A, B],
      cs: DepC[A, B, C],
      ds: DepD[A, B, C, D],
      es: DepE[A, B, C, D, E],
      fs: DepF[A, B, C, D, E, F]
  ) =
    applyStandardForm(
      a.unit(),
      bs(_).unit(),
      cs(_, _).unit(),
      ds(_, _, _).unit(),
      es(_, _, _, _).unit(),
      fs(_, _, _, _, _).unit()
    )

  infix def applyRectangular[T[_]: Mappable](as: T[A], bs: T[B], cs: T[C], ds: T[D], es: T[E], fs: T[F]) =
    val v = baseShape(as, bs, cs, ds, es, fs, f)
    v.rectComprehensionDerived[Z](id)

  /** This is equivalent to:
   *    for
   *      a <- as
   *      b <- bs(a)
   *      c <- cs(a, b)
   *      d <- ds(a, b, c)
   *      e <- es(a, b, c, d) 
   *      f0<- fs(a, b, c, d, e)
   *    yield
   *      f(a, b, c, d, e, f0)
   */
  infix def applyStandardForm[T[_]: Mappable](
      as: T[A],
      bs: DepTB[T, A, B],
      cs: DepTC[T, A, B, C],
      ds: DepTD[T, A, B, C, D],
      es: DepTE[T, A, B, C, D, E],
      fs: DepTF[T, A, B, C, D, E, F]
  ) =
    val v = VariantIrregDerivedF[T, Z, A, B, C, D, E, F, R, S](as, bs, cs, ds, es, fs, f)
    v.irregComprehensionDerived[Z](id)

  /** The "threaded" form is equivalent to:
   *    for
   *      a <- as
   *      b <- bs(a)
   *      c <- cs(b)
   *      d <- ds(c)
   *      e <- es(d)
   *      f0<- fs(e)
   *    yield
   *      f(a, b, c, d, e, f0)
   */
  def applyThreaded[T[_]: Mappable](
      as: T[A],
      bs: DepTB[T, A, B],
      cs: DepTB[T, B, C],
      ds: DepTB[T, C, D],
      es: DepTB[T, D, E],
      fs: DepTB[T, E, F]
  ) =
    applyStandardForm(as, bs, (_, b) => cs(b), (_, _, c) => ds(c), (_, _, _, d) => es(d), (_, _, _, _, e) => fs(e))

  infix def applyMixed[T[_]](
      as: => Mixed[A, T],
      bs: A => Mixed[B, T],
      cs: (A, B) => Mixed[C, T],
      ds: (A, B, C) => Mixed[D, T],
      es: (A, B, C, D) => Mixed[E, T],
      fs: (A, B, C, D, E) => Mixed[F, T]
  )(using
      m: Mappable[T],
      ta: ClassTag[A],
      tb: ClassTag[B],
      tc: ClassTag[C],
      td: ClassTag[D],
      te: ClassTag[E],
      tf: ClassTag[F],
      pa: Plain[A],
      pb: Plain[B],
      pc: Plain[C],
      pd: Plain[D],
      pe: Plain[E],
      pf: Plain[F],
      pz: Plain[Z]
  ): T[Z] =
    applyStandardForm(
      toDerived(as, m.isDelayed),
      a => toDerived(bs(a), m.isDelayed),
      (a, b) => toDerived(cs(a, b), m.isDelayed),
      (a, b, c) => toDerived(ds(a, b, c), m.isDelayed),
      (a, b, c, d) => toDerived(es(a, b, c, d), m.isDelayed),
      (a, b, c, d, e) => toDerived(fs(a, b, c, d, e), m.isDelayed)
    )

  def apply[U, V](us: Seq[U], vs: Seq[V], g: (U, V) => (A, B, C, D, E, F))(using ClassTag[Z]) =
    val v = AugmentB[MultiArrayB, SeqB]()(us, vs, g)
    v.rectComprehension(f(_, _, _, _, _, _))

trait AugmentedFnG[Z, A, B, C, D, E, F, G, R[_, _, _, _, _, _, _, _], S[_, _, _, _, _, _, _, _]](using
    cx: ComprehensionG[R],
    cy: ComprehensionG[S]
) extends AugmentedFnGBase[Z, A, B, C, D, E, F, G]:

  val baseShape = AugmentG[R, S]()

  def apply[X <: Product](x: X)(using mp: ProdG[X, A, B, C, D, E, F, G]) =
    val (a, b, c, d, e, f0, g) = Tuple.fromProductTyped(x)
    f(a, b, c, d, e, f0, g)

  def apply(as: Seq[A], bs: Seq[B], cs: Seq[C], ds: Seq[D], es: Seq[E], fs: Seq[F], gs: Seq[G])(using
      ClassTag[Z]
  ): R[Z, A, B, C, D, E, F, G] =
    val v = baseShape(as, bs, cs, ds, es, fs, gs, f)
    v.rectComprehension(id)

  def apply(
      as: Seq[A],
      bs: GenSeqB[A, B],
      cs: GenSeqC[A, B, C],
      ds: GenSeqD[A, B, C, D],
      es: GenSeqE[A, B, C, D, E],
      fs: GenSeqF[A, B, C, D, E, F],
      gs: GenSeqG[A, B, C, D, E, F, G]
  ) =
    val v = baseShape(as, seq(bs), seq(cs), seq(ds), seq(es), seq(fs), seq(gs), f)
    v.irregComprehension[Z](id)

  infix def applyPlainForm[T[_]: Mappable](
      a: => A,
      bs: DepB[A, B],
      cs: DepC[A, B, C],
      ds: DepD[A, B, C, D],
      es: DepE[A, B, C, D, E],
      fs: DepF[A, B, C, D, E, F],
      gs: DepG[A, B, C, D, E, F, G]
  ) =
    applyStandardForm(
      a.unit(),
      bs(_).unit(),
      cs(_, _).unit(),
      ds(_, _, _).unit(),
      es(_, _, _, _).unit(),
      fs(_, _, _, _, _).unit(),
      gs(_, _, _, _, _, _).unit()
    )

  infix def applyRectangular[T[_]: Mappable](as: T[A], bs: T[B], cs: T[C], ds: T[D], es: T[E], fs: T[F], gs: T[G]) =
    val v = baseShape(as, bs, cs, ds, es, fs, gs, f)
    v.rectComprehensionDerived[Z](id)

  /** This is equivalent to:
   *    for
   *      a <- as
   *      b <- bs(a)
   *      c <- cs(a, b)
   *      d <- ds(a, b, c)
   *      e <- es(a, b, c, d) 
   *      f0<- fs(a, b, c, d, e)
   *      g <- gs(a, b, c, d, e, f)
   *    yield
   *      f(a, b, c, d, e, f0, g)
   */
  infix def applyStandardForm[T[_]: Mappable](
      as: T[A],
      bs: DepTB[T, A, B],
      cs: DepTC[T, A, B, C],
      ds: DepTD[T, A, B, C, D],
      es: DepTE[T, A, B, C, D, E],
      fs: DepTF[T, A, B, C, D, E, F],
      gs: DepTG[T, A, B, C, D, E, F, G]
  ) =
    val v = VariantIrregDerivedG[T, Z, A, B, C, D, E, F, G, R, S](as, bs, cs, ds, es, fs, gs, f)
    v.irregComprehensionDerived[Z](id)

  /** The "threaded" form is equivalent to:
   *    for
   *      a <- as
   *      b <- bs(a)
   *      c <- cs(b)
   *      d <- ds(c)
   *      e <- es(d)
   *      f0<- fs(e)
   *      g <- gs(f0)
   *    yield
   *      f(a, b, c, d, e, f0, g)
   */
  def applyThreaded[T[_]: Mappable](
      as: T[A],
      bs: DepTB[T, A, B],
      cs: DepTB[T, B, C],
      ds: DepTB[T, C, D],
      es: DepTB[T, D, E],
      fs: DepTB[T, E, F],
      gs: DepTB[T, F, G]
  ) =
    applyStandardForm(
      as,
      bs,
      (_, b) => cs(b),
      (_, _, c) => ds(c),
      (_, _, _, d) => es(d),
      (_, _, _, _, e) => fs(e),
      (_, _, _, _, _, f0) => gs(f0)
    )

  infix def applyMixed[T[_]](
      as: => Mixed[A, T],
      bs: A => Mixed[B, T],
      cs: (A, B) => Mixed[C, T],
      ds: (A, B, C) => Mixed[D, T],
      es: (A, B, C, D) => Mixed[E, T],
      fs: (A, B, C, D, E) => Mixed[F, T],
      gs: (A, B, C, D, E, F) => Mixed[G, T]
  )(using
      m: Mappable[T],
      ta: ClassTag[A],
      tb: ClassTag[B],
      tc: ClassTag[C],
      td: ClassTag[D],
      te: ClassTag[E],
      tf: ClassTag[F],
      tg: ClassTag[G],
      pa: Plain[A],
      pb: Plain[B],
      pc: Plain[C],
      pd: Plain[D],
      pe: Plain[E],
      pf: Plain[F],
      pg: Plain[G],
      pz: Plain[Z]
  ): T[Z] =
    applyStandardForm(
      toDerived(as),
      a => toDerived(bs(a)),
      (a, b) => toDerived(cs(a, b)),
      (a, b, c) => toDerived(ds(a, b, c)),
      (a, b, c, d) => toDerived(es(a, b, c, d)),
      (a, b, c, d, e) => toDerived(fs(a, b, c, d, e)),
      (a, b, c, d, e, f) => toDerived(gs(a, b, c, d, e, f))
    )

  def apply[U, V](us: Seq[U], vs: Seq[V], g: (U, V) => (A, B, C, D, E, F, G))(using ClassTag[Z]) =
    val v = AugmentB[MultiArrayB, SeqB]()(us, vs, g)
    v.rectComprehension(f(_, _, _, _, _, _, _))
