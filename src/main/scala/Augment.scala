package augmented

import basicdef._
import comprehension._
import comprehension.given
import mappable._
import mappable.given
import multiarray._
import shape._
import util.timed
import util.JavaUtil.{Pair, Triple}
import variant._

import collection.JavaConverters._
import scala.reflect.ClassTag

given AugmentA[ArrayNA, SeqA] = AugmentA()
given AugmentB[MultiArrayB, SeqB] = AugmentB()
given AugmentC[MultiArrayC, SeqC] = AugmentC()
given AugmentD[MultiArrayD, SeqD] = AugmentD()
given AugmentE[MultiArrayE, SeqE] = AugmentE()

given Effects[BasicIO] = Effects()

object augment:
  
  def apply[Z, A, R[_, _], S[_, _]]                                     (f: A => Z)               (using aug: AugmentA[R, S]) = aug (f)
  def apply[Z, A, B, R[_, _, _], S[_, _, _]]                            (f: (A, B) => Z)          (using aug: AugmentB[R, S]) = aug (f)
  def apply[Z, A, B, C, R[_, _, _, _], S[_, _, _, _]]                   (f: (A, B, C) => Z)       (using aug: AugmentC[R, S]) = aug (f)
  def apply[Z, A, B, C, D, R[_, _, _, _, _], S[_, _, _, _, _]]          (f: (A, B, C, D) => Z)    (using aug: AugmentD[R, S]) = aug (f)
  def apply[Z, A, B, C, D, E, R[_, _, _, _, _, _], S[_, _, _, _, _, _]] (f: (A, B, C, D, E) => Z) (using aug: AugmentE[R, S]) = aug (f)
  
  def flat[Z, A]              (f: A => Z)               = AugmentA[SeqA, SeqA]() (f)
  def flat[Z, A, B]           (f: (A, B) => Z)          = AugmentB[SeqB, SeqB]() (f)
  def flat[Z, A, B, C]        (f: (A, B, C) => Z)       = AugmentC[SeqC, SeqC]() (f)
  def flat[Z, A, B, C, D]     (f: (A, B, C, D) => Z)    = AugmentD[SeqD, SeqD]() (f)
  def flat[Z, A, B, C, D, E]  (f: (A, B, C, D, E) => Z) = AugmentE[SeqE, SeqE]() (f)

  def seq[Z, A]               (f: A => Z)               = AugmentA[SeqNA, SeqNA]() (f)
  def seq[Z, A, B]            (f: (A, B) => Z)          = AugmentB[SeqNB, SeqNB]() (f)
  def seq[Z, A, B, C]         (f: (A, B, C) => Z)       = AugmentC[SeqNC, SeqNC]() (f)
  def seq[Z, A, B, C, D]      (f: (A, B, C, D) => Z)    = AugmentD[SeqND, SeqND]() (f)
  def seq[Z, A, B, C, D, E]   (f: (A, B, C, D, E) => Z) = AugmentE[SeqNE, SeqNE]() (f)

  def mapped[Z, A]            (f: A => Z)               = AugmentA[MapA, SeqA]() (f)
  def mapped[Z, A, B]         (f: (A, B) => Z)          = AugmentB[MapB, SeqB]() (f)
  def mapped[Z, A, B, C]      (f: (A, B, C) => Z)       = AugmentC[MapC, SeqC]() (f)
  def mapped[Z, A, B, C, D]   (f: (A, B, C, D) => Z)    = AugmentD[MapD, SeqD]() (f)
  def mapped[Z, A, B, C, D, E](f: (A, B, C, D, E) => Z) = AugmentE[MapE, SeqE]() (f)

  def set[Z, A]               (f: A => Z)               = AugmentA[SetA, SetA]() (f)
  def set[Z, A, B]            (f: (A, B) => Z)          = AugmentB[SetB, SetB]() (f)
  def set[Z, A, B, C]         (f: (A, B, C) => Z)       = AugmentC[SetC, SetC]() (f)
  def set[Z, A, B, C, D]      (f: (A, B, C, D) => Z)    = AugmentD[SetD, SetD]() (f)
  def set[Z, A, B, C, D, E]   (f: (A, B, C, D, E) => Z) = AugmentE[SetE, SetE]() (f)
  
  def apply[Z, A]             (f: A => Z, c: Conditions[Z, A])                            = AugmentA[ArrayNA, SeqA]()(f, c)
  def apply[Z, A, B]          (f: (A, B) => Z, c: Conditions[Z, (A, B)])                  = AugmentB[MultiArrayB, SeqB]()(f, c)
  def apply[Z, A, B, C]       (f: (A, B, C) => Z, c: Conditions[Z, (A, B, C)])            = AugmentC[MultiArrayC, SeqC]()(f, c)
  def apply[Z, A, B, C, D]    (f: (A, B, C, D) => Z, c: Conditions[Z, (A, B, C, D)])      = AugmentD[MultiArrayD, SeqD]()(f, c)
  def apply[Z, A, B, C, D, E] (f: (A, B, C, D, E) => Z, c: Conditions[Z, (A, B, C, D, E)])= AugmentE[MultiArrayE, SeqE]()(f, c)

  def apply[Z, A]             (f: A => Z, preCond: Condition[A], postCond: Condition[(Z, A)]) =
    AugmentA[ArrayNA, SeqA]()(f, Conditions(preCond, postCond))

object image:
  def apply[Z, A](as: Seq[A], f: A => Z)(using ClassTag[Z]) =
    augment(f)(as)
  def apply[Z, A](as: Seq[A], phi: A => Boolean, f: A => Z)(using ClassTag[Z]) =
    augment(f)(as, phi)

  def apply[Z, A, B](f: (A, B) => Z, as: Seq[A], bs: Seq[B])(using ClassTag[Z]) =
    augment(f)(as, bs)
  def apply[Z, A, B](as: Seq[A], bs: Seq[B], f: (A, B) => Z)(using ClassTag[Z]) =
    augment(f)(as, bs)

  def apply[Z, A, B](f: (A, B) => Z, as: Seq[A], bs: GenSeqB[A, B]) =
    augment(f)(as, bs)
  def apply[Z, A, B](as: Seq[A], bs: GenSeqB[A, B], f: (A, B) => Z) =
    augment(f)(as, bs)

  def apply[Z, A, B](as: Seq[A], bs: GenSeqB[A, B], phi: (A, B) => Boolean, f: (A, B) => Z) =
    augment(f)(as, bs, phi)

  def apply[Z, A, B, C](f: (A, B, C) => Z, as: Seq[A], bs: Seq[B], cs: Seq[C])(using ClassTag[Z]) =
    augment(f)(as, bs, cs)

  def apply[Z, A, B, C](as: Seq[A], bs: Seq[B], cs: Seq[C], f: (A, B, C) => Z)(using ClassTag[Z]) =
    augment(f)(as, bs, cs)

  def apply[Z, A, B, C](as: Seq[A], bs: DepSeqB[A, B], cs: DepSeqB[B, C], f: (A, B, C) => Z)(using
      ClassTag[Z]
  ): Seq[Z] =
    apply(as, bs, (_, b) => cs(b), f)

  def apply[Z, A, B, C](as: Seq[A], bs: DepSeqB[A, B], cs: DepSeqC[A, B, C], f: (A, B, C) => Z)(using ClassTag[Z]) =
    augment(f)(as, bs, cs)

  def apply[T[_], Z, A, B, C](as: T[A], bs: DepTB[T, A, B], cs: DepTC[T, A, B, C], f: (A, B, C) => Z)(using
      Mappable[T]
  ) =
    augment(f)(as, bs, cs)

  def apply[Z, A, B, C, D](f: (A, B, C, D) => Z, as: Seq[A], bs: Seq[B], cs: Seq[C], ds: Seq[D])(using ClassTag[Z]) =
    augment(f)(as, bs, cs, ds)

  def apply[Z, A, B, C, D](as: Seq[A], bs: Seq[B], cs: Seq[C], ds: Seq[D], f: (A, B, C, D) => Z)(using ClassTag[Z]) =
    augment(f)(as, bs, cs, ds)

  def apply[Z, A, B, C, D, E](f: (A, B, C, D, E) => Z, as: Seq[A], bs: Seq[B], cs: Seq[C], ds: Seq[D], es: Seq[E])(using
      ClassTag[Z]
  ) =
    augment(f)(as, bs, cs, ds, es)

  def apply[Z, A, B, C, D, E](as: Seq[A], bs: Seq[B], cs: Seq[C], ds: Seq[D], es: Seq[E], f: (A, B, C, D, E) => Z)(using
      ClassTag[Z]
  ) =
    augment(f)(as, bs, cs, ds, es)

  def apply[Z, A, B, C, R[_, _, _, _], S[_, _, _, _]](
      f: (A, B, C) => Z,
      as: Seq[A],
      bs: GenSeqB[A, B],
      cs: GenSeqC[A, B, C]
  )(using aug: AugmentC[R, S]) =
    aug(f)(as, bs, cs)

  def apply[Z, A, B, C](
      f: (A, B, C) => Z,
      as: Seq[A],
      bs: GenSeqB[A, B],
      cs: GenSeqB[B, C],
      phi: (A, B, C) => Boolean
  ) =
    augment(f)(as, bs, cs, phi)

  def apply[T[_]: Mappable, Z, A, B](as: T[A], bs: T[B], f: (A, B) => Z)(using ClassTag[Z]) =
    augment(f) applyT (as, bs)

  def apply[T[_]: Mappable, Z, A, B, C](as: T[A], bs: T[B], cs: T[C], f: (A, B, C) => Z)(using ClassTag[Z]) =
    augment(f) applyT (as, bs, cs)

object apply:

  def apply[T[_]: Mappable, U[_]: Mappable, Z, A, B](as: => T[A], bs: => U[B], f: (A, B) => Z, z: Boolean = true)(using
      ClassTag[A],
      ClassTag[B]
  ): T[U[Z]] =
    augment(f) applyU (as, bs)

  def apply[T[_]: Mappable, U[_]: Mappable, Z, A, B](as: => T[U[A]], bs: => T[U[B]], f: (A, B) => Z)(using
      ClassTag[A],
      ClassTag[B]
  ): T[U[Z]] =
    augment(f) applyTU (as, bs)

  def apply[T[_]: Mappable, U[_]: Mappable, V[_]: Mappable, Z, A, B, C](
      as: => T[A],
      bs: => U[B],
      cs: V[C],
      f: (A, B, C) => Z
  )(using
      ClassTag[A],
      ClassTag[B],
      ClassTag[C]
  ): T[U[V[Z]]] =
    augment(f) applyV (as, bs, cs)

case class Effects[T[_]]()(using Mappable[T]):
  def apply() = ()

object select:

  def apply[A, B](as: Seq[A], bs: Seq[B]) =
    image(tupledB, as, bs)

  def apply[A, B](as: Seq[A], bs: GenSeqB[A, B]) =
    image(tupledB, as, bs)

  def apply[A, B](as: Seq[A], bs: GenSeqB[A, B], phi: (A, B) => Boolean = (a: A, b: B) => true) =
    image(tupledB, as, a => seq(bs)(a).filter(phi(a, _)))

  def apply[A, B, C](as: Seq[A], bs: Seq[B], cs: Seq[C]) =
    image(tupledC, as, bs, cs)

  def apply[A, B, C](as: Seq[A], bs: GenSeqB[A, B], cs: GenSeqC[A, B, C]) =
    image(tupledC, as, bs, seq(cs)(_, _))

  def apply[A, B, C](as: Seq[A], bs: GenSeqB[A, B], cs: GenSeqB[B, C]) =
    image(tupledC, as, bs, (a, b) => seq(cs)(b))

  def apply[A, B, C, R[_, _, _, _], S[_, _, _, _]](
      as: Seq[A],
      bs: GenSeqB[A, B],
      cs: GenSeqB[B, C],
      phi: (A, B, C) => Boolean
  )(using aug: AugmentC[R, S]) =
    image(tupledC, as, bs, (a, b) => seq(cs)(b).filter(phi(a, b, _)))

  def apply[A, B, C](as: Set[A], bs: DepSetB[A, B], cs: DepSetB[B, C]) =
    tupleC applyS (as, bs, (_, b) => cs(b))

  def apply[T[_], A, B, C](as: T[A], bs: T[B], cs: T[C])(using t: Mappable[T]) =
    tupleC applyT (as, bs, cs)

object count:
  def apply[A, B, C](as: Seq[A], bs: DepSeqB[A, B], cs: DepSeqB[B, C]) =
    image(tupledC, as, bs, (_, b) => cs(b)).length

object sequence:

  def apply[T[_], A, B, C](a: => A, b: => B, cs: B => C, z: Boolean = true)(using fx: Effects[T], t: Mappable[T]) =
    lastC applyAC (a.unit(), a => b.unit(), (_, b) => cs(b).unit())

  def apply[T[_], A, B, C](a: => A, bs: A => B, cs: B => C)(using fx: Effects[T], t: Mappable[T]) =
    lastC applyAC (a.unit(), bs(_).unit(), (_, b) => cs(b).unit())

  def apply[T[_], A, B, C](a: => A, b: => B, cs: (A, B) => C)(using fx: Effects[T], t: Mappable[T]) =
    lastC applyC (a.unit(), b.unit(), cs(_, _).unit())

  def apply[T[_], A, B, C, D](a: => A, bs: A => B, cs: (A, B) => C, ds: (A, B, C) => D)(using
      fx: Effects[T],
      t: Mappable[T]
  ) =
    lastD applyD (a.unit(), bs(_).unit(), cs(_, _).unit(), ds(_, _, _).unit())

  def apply[T[_], A, B, C, D](a: => A, b: => B, cs: (A, B) => C, ds: C => D)(using
      fx: Effects[T],
      t: Mappable[T]
  ) =
    lastD applyD (a.unit(), a => b.unit(), cs(_, _).unit(), (a, b, c) => ds(c).unit())

  def apply[T[_], A, B, C, D, E](a: => A, bs: A => B, cs: B => C, ds: C => D, es: D => E)(using
      fx: Effects[T],
      t: Mappable[T]
  ) =
    lastE applyTD (a.unit(), bs(_).unit(), cs(_).unit(), ds(_).unit(), es(_).unit())

  def apply[T[_], A, B, C](as: T[A], bs: T[B], cs: T[C])(using t: Mappable[T]) =
    lastC applyT (as, bs, cs)

  def apply[T[_], A, B, C, D](a: => A, bs: A => B, cs: B => C, ds: C => D)(using fx: Effects[T], t: Mappable[T]) =
    lastD applyCD (a.unit(), bs(_).unit(), cs(_).unit(), ds(_).unit())

  def apply[T[_], A, B, C, D](as: T[A], bs: T[B], cs: (A, B) => C, ds: C => T[D])(using t: Mappable[T]) =
    lastD applyD (as, _ => bs, cs(_, _).unit(), (a, b, c) => ds(c))

  def apply[T[_], A, B, C, D, E](as: T[A], bs: T[B], cs: T[C], ds: T[D], es: T[E])(using t: Mappable[T]) =
    lastE applyT (as, bs, cs, ds, es)

  def apply[T[_], A, B, C, D, E](as: => T[A], bs: => T[B], cs: => T[C], ds: (A, B, C) => D, es: D => T[E])(using
      t: Mappable[T]
  ) =
    lastE applyTE (as, _ => bs, (_, _) => cs, ds(_, _, _).unit(), (_, _, _, d) => es(d))

  def apply[T[_], A, B, C, D, E](as: T[A], bs: T[B], cs: T[C], ds: (B, C) => D, es: D => T[E])(using t: Mappable[T]) =
    lastE applyTE (as, _ => bs, (_, _) => cs, (_, b, c) => ds(b, c).unit(), (_, _, _, d) => es(d))

object last:

  def apply[T[_], A, B, C](as: T[A], bs: T[B], cs: (A, B) => T[C])(using t: Mappable[T]) =
    lastC applyC (as, bs, cs)

  def apply[T[_], A, B, C, D](a: => A, bs: A => B, cs: B => C, ds: C => D)(using fx: Effects[T], t: Mappable[T]) =
    lastD applyCD (a.unit(), bs(_).unit(), cs(_).unit(), ds(_).unit())

// used from Java
object augmentJ:

  def augment[Z, A](f: java.util.function.Function[A, Z]) =
    AugmentA[MultiArrayA, SeqA]()((a: A) => f(a))
  def augment[Z, A, B](f: java.util.function.BiFunction[A, B, Z]) =
    AugmentB[MultiArrayB, SeqB]()((a: A, b: B) => f(a, b))
  def augment[Z, A, B, C](f: (A, B, C) => Z) = AugmentC[MultiArrayC, SeqC]()(f)
  def augment[Z, A, B, C, D](f: (A, B, C, D) => Z) = AugmentD[MultiArrayD, SeqD]()(f)
  def augment[Z, A, B, C, D, E](f: (A, B, C, D, E) => Z) = AugmentE[MultiArrayE, SeqE]()(f)

  def select[A, B, C](as: JList[A], bs: JList[B], cs: JList[C], phi: (A, B, C) => Boolean) =
    tupleJTriple applyJ (as, (a: A) => bs, (b: B) => cs, phi)

  def select[A, B, C](as: JList[A], bs: JDepSeq[A, B], phi: (A, B) => Boolean) =
    tupleJPair(as, bs, phi)

  def select[A, B, C](as: JList[A], bs: JDepSeq[A, B], cs: JDepSeq[B, C]) =
    tupleJTriple applyJ (as, bs, cs)

  def select[A, B, C](as: JList[A], bs: JDepSeq[A, B], cs: JDepSeq[B, C], phi: (A, B, C) => Boolean) =
    tupleJTriple applyJ (as, bs, cs, phi)

  def count[A, B, C](as: JList[A], bs: JDepSeq[A, B], cs: JDepSeq[B, C]) =
    (tupleJTriple applyJ (as, bs, cs)).size()

  def sequence[B](as: java.lang.Runnable, b: => B, cs: java.util.function.Consumer[B]) =
    MappableW(augmented.sequence(as.run(), _ => b, cs.accept(_)))

  def sequence[A, B, C](a: => A, b: => B, cs: B => C) =
    MappableW(augmented.sequence(a, _ => b, cs))

  def sequence[A, B, C](a: => A, bs: A => B, cs: B => C) =
    MappableW(augmented.sequence(a, bs(_), cs))

  def sequence[B, C, D](as: java.lang.Runnable, b: => B, cs: B => C, ds: C => D) =
    MappableW(augmented.sequence(as.run(), _ => b, (a, b) => cs(b), (a, b, c) => ds(c)))

  def sequence[A, B, C, D](a: => A, b: => B, cs: B => C, ds: C => D) =
    MappableW(augmented.sequence(a, _ => b, (a, b) => cs(b), (a, b, c) => ds(c)))

  def sequence[A, B, C, D](a: => A, bs: A => B, cs: B => C, ds: C => D) =
    MappableW(augmented.sequence(a, bs(_), (a, b) => cs(b), (a, b, c) => ds(c)))

def tupledB[A, B] = (a: A, b: B) => (a, b)
def tupledC[A, B, C] = (a: A, b: B, c: C) => (a, b, c)
def tupledJPair[A, B] = (a: A, b: B) => Pair(a, b)
def tupledJTriple[A, B, C] = (a: A, b: B, c: C) => Triple(a, b, c)

def tupleB[A, B] = augment((a: A, b: B) => (a, b))
def tupleC[A, B, C] = augment((a: A, b: B, c: C) => (a, b, c))
def tupleD[A, B, C, D] = augment((a: A, b: B, c: C, d: D) => (a, b, c, d))
def tupleE[A, B, C, D, E] = augment((a: A, b: B, c: C, d: D, e: E) => (a, b, c, d, e))

def tupleJPair[A, B] = augment((a: A, b: B) => Pair(a, b))
def tupleJTriple[A, B, C] = augment((a: A, b: B, c: C) => Triple(a, b, c))

def lastB[A, B] = augment((a: A, b: B) => b)
def lastC[A, B, C] = augment((a: A, b: B, c: C) => c)
def lastD[A, B, C, D] = augment((a: A, b: B, c: C, d: D) => d)
def lastE[A, B, C, D, E] = augment((a: A, b: B, c: C, d: D, e: E) => e)

type JDepSeq[A, B] = A => JList[B]
type JDepSeqC[A, B, C] = (A, B) => JList[C]

case class AugmentA[R[_, _], S[_, _]]()(using cx: ComprehensionA[R], cy: ComprehensionA[S]):
  def apply[Z, A](f: A => Z) = AugmentedFunctionA[Z, A, R, S](f)
  def apply[Z, A](f: A => Z, c: Conditions[Z, A]) = AugmentedFunctionA[Z, A, R, S](c exec (f, _))
  def apply[Z, A](as: Seq[A], f: A => Z) = VariantRectA[Z, A, R, S](as, f)
  def apply[Z, A](as: Seq[A], phi: A => Boolean, f: A => Z) = VariantIrregA[Z, A, R, S](as, phi, f)

case class AugmentB[R[_, _, _], S[_, _, _]]()(using cx: ComprehensionB[R], cy: ComprehensionB[S]):
  def apply[Z, A, B](f: (A, B) => Z) = AugmentedFunctionB[Z, A, B, R, S](f)
  def apply[Z, A, B](f: (A, B) => Z, cond: Conditions[Z, (A, B)]) =
    AugmentedFunctionB[Z, A, B, R, S]((a, b) => cond exec (f.tupled, (a, b)))
  def apply[Z, A, B](as: Seq[A], bs: Seq[B], f: (A, B) => Z) = VariantRectB[Z, A, B, R, S](as, bs, f)
  def apply[Z, A, B](as: Seq[A], bsGen: GenSeqB[A, B], f: (A, B) => Z) = VariantIrregB[Z, A, B, R, S](as, seq(bsGen), f)

  def apply[T[_], Z, A, B](as: => T[A], bs: => T[B], f: (A, B) => Z)(using Mappable[T]) =
    VariantRectDerivedB[T, Z, A, B, R, S](as, bs, f)

case class AugmentC[R[_, _, _, _], S[_, _, _, _]]()(using cx: ComprehensionC[R], cy: ComprehensionC[S]):
  def apply[Z, A, B, C](f: (A, B, C) => Z) = AugmentedFunctionC[Z, A, B, C, R, S](f)
  def apply[Z, A, B, C](f: (A, B, C) => Z, cond: Conditions[Z, (A, B, C)]) =
    AugmentedFunctionC[Z, A, B, C, R, S]((a, b, c) => cond exec (f.tupled, (a, b, c)))
  def apply[Z, A, B, C](as: Seq[A], bs: Seq[B], cs: Seq[C], f: (A, B, C) => Z) =
    VariantRectC[Z, A, B, C, R, S](as, bs, cs, f)
  def apply[Z, A, B, C](as: Seq[A], bsGen: GenSeqB[A, B], csGen: GenSeqC[A, B, C], f: (A, B, C) => Z) =
    VariantIrregC[Z, A, B, C, R, S](as, seq(bsGen), seq(csGen), f)
  def apply[Z, A, B, C](as: Seq[A], bsGen: DepSeqB[A, B], csGen: DepSeqC[A, B, C], f: (A, B, C) => Z) =
    VariantIrregC[Z, A, B, C, R, S](as, bsGen, csGen, f)

  def apply[T[_], Z, A, B, C](as: => T[A], bs: => T[B], cs: => T[C], f: (A, B, C) => Z)(using Mappable[T]) =
    VariantRectDerivedC[T, Z, A, B, C, R, S](as, bs, cs, f)

  def apply[T[_], Z, A, B, C](as: T[A], bsDep: DepTB[T, A, B], csDep: DepTC[T, A, B, C], f: (A, B, C) => Z)(using
      Mappable[T]
  ) =
    VariantIrregDerivedC[T, Z, A, B, C, R, S](as, bsDep, csDep, f)

  def apply[T[_], Z, A, B, C](as: T[A], bs: T[B], csDep: DepTB[T, B, C], f: (A, B, C) => Z)(using Mappable[T]) =
    VariantIrregDerivedC[T, Z, A, B, C, R, S](as, a => bs, (a, b) => csDep(b), f)

  def apply[T[_], Z, A, B, C](as: T[A], bsDep: DepTB[T, A, B], csDep: DepTB[T, B, C], f: (A, B, C) => Z)(using
      Mappable[T]
  ) =
    VariantIrregDerivedC[T, Z, A, B, C, R, S](as, bsDep, (a, b) => csDep(b), f)

case class AugmentD[R[_, _, _, _, _], S[_, _, _, _, _]]()(using cx: ComprehensionD[R], cy: ComprehensionD[S]):
  def apply[Z, A, B, C, D](f: (A, B, C, D) => Z) = AugmentedFunctionD[Z, A, B, C, D, R, S](f)
  def apply[Z, A, B, C, D](f: (A, B, C, D) => Z, cond: Conditions[Z, (A, B, C, D)]) =
    AugmentedFunctionD[Z, A, B, C, D, R, S]((a, b, c, d) => cond exec (f.tupled, (a, b, c, d)))
  def apply[Z, A, B, C, D](as: Seq[A], bs: Seq[B], cs: Seq[C], ds: Seq[D], f: (A, B, C, D) => Z) =
    VariantRectD[Z, A, B, C, D, R, S](as, bs, cs, ds, f)
  def apply[Z, A, B, C, D](
      as: Seq[A],
      bsDep: DepSeqB[A, B],
      csDep: DepSeqC[A, B, C],
      dsDep: DepSeqD[A, B, C, D],
      f: (A, B, C, D) => Z
  ) =
    VariantIrregD[Z, A, B, C, D, R, S](as, bsDep, csDep, dsDep, f)

  def apply[T[_], Z, A, B, C, D](
      as: T[A],
      bsDep: DepTB[T, A, B],
      csDep: DepTB[T, B, C],
      dsDep: DepTB[T, C, D],
      f: (A, B, C, D) => Z
  )(using Mappable[T]) =
    VariantIrregDerivedD[T, Z, A, B, C, D, R, S](as, bsDep, (a, b) => csDep(b), (a, b, c) => dsDep(c), f)

  def apply[T[_], Z, A, B, C, D](
      as: T[A],
      bsDep: DepTB[T, A, B],
      csDep: DepTC[T, A, B, C],
      dsDep: DepTD[T, A, B, C, D],
      f: (A, B, C, D) => Z
  )(using Mappable[T]) =
    VariantIrregDerivedD[T, Z, A, B, C, D, R, S](as, bsDep, csDep, dsDep, f)

  def apply[T[_], Z, A, B, C, D](as: => T[A], bs: => T[B], cs: => T[C], ds: => T[D], f: (A, B, C, D) => Z)(using
      Mappable[T]
  ) =
    VariantRectDerivedD[T, Z, A, B, C, D, R, S](as, bs, cs, ds, f)

case class AugmentE[R[_, _, _, _, _, _], S[_, _, _, _, _, _]]()(using cx: ComprehensionE[R], cy: ComprehensionE[S]):
  def apply[Z, A, B, C, D, E](f: (A, B, C, D, E) => Z) = AugmentedFunctionE[Z, A, B, C, D, E, R, S](f)
  def apply[Z, A, B, C, D, E](f: (A, B, C, D, E) => Z, cond: Conditions[Z, (A, B, C, D, E)]) =
    AugmentedFunctionE[Z, A, B, C, D, E, R, S]((a, b, c, d, e) => cond exec (f.tupled, (a, b, c, d, e)))
  def apply[Z, A, B, C, D, E](as: Seq[A], bs: Seq[B], cs: Seq[C], ds: Seq[D], es: Seq[E], f: (A, B, C, D, E) => Z) =
    VariantRectE[Z, A, B, C, D, E, R, S](as, bs, cs, ds, es, f)
  def apply[Z, A, B, C, D, E](
      as: Seq[A],
      bsGen: DepSeqB[A, B],
      csGen: DepSeqC[A, B, C],
      dsGen: DepSeqD[A, B, C, D],
      esGen: DepSeqE[A, B, C, D, E],
      f: (A, B, C, D, E) => Z
  ) =
    VariantIrregE[Z, A, B, C, D, E, R, S](as, bsGen, csGen, dsGen, esGen, f)

  def apply[T[_], Z, A, B, C, D, E](
      as: => T[A],
      bs: => T[B],
      cs: => T[C],
      ds: => T[D],
      es: => T[E],
      f: (A, B, C, D, E) => Z
  )(using Mappable[T]) =
    VariantRectDerivedE[T, Z, A, B, C, D, E, R, S](as, bs, cs, ds, es, f)

  def apply[T[_], Z, A, B, C, D, E](
      as: T[A],
      bsDep: DepTB[T, A, B],
      csDep: DepTB[T, B, C],
      dsDep: DepTB[T, C, D],
      esDep: DepTB[T, D, E],
      f: (A, B, C, D, E) => Z
  )(using Mappable[T]) =
    VariantIrregDerivedE[T, Z, A, B, C, D, E, R, S](
      as,
      bsDep,
      (a, b) => csDep(b),
      (a, b, c) => dsDep(c),
      (a, b, c, d) => esDep(d),
      f
    )

  def apply[T[_], Z, A, B, C, D, E](
      as: T[A],
      bsDep: DepTB[T, A, B],
      csDep: DepTC[T, A, B, C],
      dsDep: DepTD[T, A, B, C, D],
      esDep: DepTE[T, A, B, C, D, E],
      f: (A, B, C, D, E) => Z
  )(using Mappable[T]) =
    VariantIrregDerivedE[T, Z, A, B, C, D, E, R, S](
      as,
      bsDep,
      (a, b) => csDep(a, b),
      (a, b, c) => dsDep(a, b, c),
      (a, b, c, d) => esDep(a, b, c, d),
      f
    )

case class AugmentedFunctionA[Z, A, R[_, _], S[_, _]](f: A => Z)(using cx: ComprehensionA[R], cy: ComprehensionA[S])
    extends AugmentedFnA[Z, A, R, S]
case class AugmentedFunctionB[Z, A, B, R[_, _, _], S[_, _, _]](f: (A, B) => Z)(using
    cx: ComprehensionB[R],
    cy: ComprehensionB[S]
) extends AugmentedFnB[Z, A, B, R, S]
case class AugmentedFunctionC[Z, A, B, C, R[_, _, _, _], S[_, _, _, _]](f: (A, B, C) => Z)(using
    cx: ComprehensionC[R],
    cy: ComprehensionC[S]
) extends AugmentedFnC[Z, A, B, C, R, S]
case class AugmentedFunctionD[Z, A, B, C, D, R[_, _, _, _, _], S[_, _, _, _, _]](f: (A, B, C, D) => Z)(using
    cx: ComprehensionD[R],
    cy: ComprehensionD[S]
) extends AugmentedFnD[Z, A, B, C, D, R, S]
case class AugmentedFunctionE[Z, A, B, C, D, E, R[_, _, _, _, _, _], S[_, _, _, _, _, _]](f: (A, B, C, D, E) => Z)(using
    cx: ComprehensionE[R],
    cy: ComprehensionE[S]
) extends AugmentedFnE[Z, A, B, C, D, E, R, S]

def id[A](a: A) = a

trait AugmentedFnABase[Z, A]:
  def f: A => Z
  def apply(a: A) =
    f(a)

trait AugmentFnBBase[Z, A, B]:
  def f: (A, B) => Z
  def apply(a: A, b: B) =
    f(a, b)

trait AugmentedFnCBase[Z, A, B, C]:
  def f: (A, B, C) => Z
  def apply(a: A, b: B, c: C) =
    f(a, b, c)

trait AugmentedFnDBase[Z, A, B, C, D]:
  def f: (A, B, C, D) => Z
  def apply(a: A, b: B, c: C, d: D) =
    f(a, b, c, d)

trait AugmentedFnEBase[Z, A, B, C, D, E]:
  def f: (A, B, C, D, E) => Z
  def apply(a: A, b: B, c: C, d: D, e: E) =
    f(a, b, c, d, e)

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

  def apply[T[_]](as: => Mixed[A, T], bs: => Mixed[B, T], z: Boolean = true)(using
      m: Mappable[T],
      tagA: ClassTag[A],
      tagB: ClassTag[B]
  ): T[Z] =
    if m.isDelayed then cx.rectDerived(as.toDerivedFromMixed(), toDerived(bs), f, id)
    else cx.rectDerived(toDerived(as), toDerived(bs), f, id)

  def applyT[T[_]](as: T[A], bs: T[B])(using Mappable[T]) =
    val v = baseShape(as, bs, f)
    v.rectComprehensionDerived[Z](id)

  def apply(as: MappableT[A], b: B) = as.getFnValueB1(f, b)
  def apply(a: A, bs: MappableT[B]) = bs.getFnValueB2(f, a)

  def applyMX[T[_]](as: => Mixed[A, T], bs: => Mixed[B, T], z: Boolean = true)(using
      Mappable[T],
      ClassTag[A],
      ClassTag[B]
  ) =
    apply(as, bs, z)

  def applyU[T[_]: Mappable, U[_]: Mappable](as: => T[A], bs: => U[B]): T[U[Z]] =
    val y = VariantRectDerivedMultipleB[T, U, Z, A, B, R, S](as, bs, f)
    y.rectComprehensionDerivedMultiple()

  def applyTU[T[_]: Mappable, U[_]: Mappable](as: => T[U[A]], bs: => T[U[B]])(using
      ClassTag[A],
      ClassTag[B]
  ): T[U[Z]] =
    as.flatMap: a =>
      bs.map: b =>
        augment(f)(a, b)

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

  def apply(as: ColVector[A], bs: ColVector[B]): Vector[Z] =
    (as.vec zip bs.vec).map(f(_, _))

  def crossCheck(g: (Seq[A], Seq[B]) => Seq[Z], as: Seq[A], bs: Seq[B], showFull: Boolean = true) =
    val (r, t) = timed(g(as, bs))

    val vf = AugmentB[SeqB, SeqB]()(as, bs, f)
    val (rf, tf) = timed(vf.irregComprehension[Z](id))
    displayCheckRes(r, rf, t, tf, showFull)
    assert(r == rf)
    (r, rf)

  def crossCheck(as: Seq[A], bs: Seq[B], g: (Seq[A], Seq[B]) => Seq[Z]): (Seq[Z], Seq[Z]) =
    crossCheck(g, as, bs)

  def crossCheck(as: Seq[A], bs: DepSeqB[A, B], g: (Seq[A], DepSeqB[A, B]) => Seq[Z]): Unit =
    val (r, t) = timed(g(as, bs))
    val vf = AugmentB[SeqB, SeqB]()(as, bs, f)
    val (rf, tf) = timed(vf.irregComprehension[Z](id))
    displayCheckRes(r, rf, t, tf, true)
    (r, rf)

  def crossCheck(as: Seq[A], bs: GenSeqB[A, B], r: Seq[Z]) =
    val vf = AugmentB[SeqB, SeqB]()(as, bs, f)
    val (rf, tf) = timed(vf.irregComprehension[Z](id))
    displayCheckRes(r, rf, 0, tf, true)
    assert(r == rf)
    (r, rf)

  def crossCheck(as: Set[A], bs: Set[B], g: (Set[A], Set[B]) => Set[Z])(using ClassTag[Z]): Unit =
    crossCheck(g, as, bs)

  def crossCheck(g: (Set[A], Set[B]) => Set[Z], as: Set[A], bs: Set[B])(using ClassTag[Z]) =
    val (r, t) = timed(g(as, bs))
    val vf = VariantSetRectangularB[Z, A, B](as, bs, f)
    val (rf, tf) = timed(vf.rectComprehension[Z](id))
    println(f"Augment (sets):\t[t: $tf%7.0f]\n")
    assert(r == rf)

  def crossCheckFX[T[_]](as: => Mixed[A, T], bs: => Mixed[B, T], r: T[Z])(using
      Mappable[T],
      ClassTag[A],
      ClassTag[B]
  ): (T[Z], T[Z]) =
    val (rf, tf) = timed(apply(as, bs))
    assert(r == rf)
    (r, rf)

def displayCheckRes[A](orig: A, augmented: A, origTm: Double, augmentedTm: Double, showFull: Boolean) =
  if showFull then
    println(f"\nOrig:\n\t${orig}" + (if origTm > 0 then f"\n\t[t: $origTm%7.0f]" else ""))
    println(f"Augmented:\n\t${augmented}" + (if origTm > 0 then f"\n\t[t: $augmentedTm%7.0f]\n" else ""))
  else
    println(f"\nOrig:\t[t: $origTm%7.0f]")
    println(f"Augmented:\t[t: $augmentedTm%7.0f]\n")

trait AugmentedFnC[Z, A, B, C, R[_, _, _, _], S[_, _, _, _]](using cr: ComprehensionC[R], cs: ComprehensionC[S])
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

  def applyS(as: Set[A], bs: DepSetB[A, B], cs: DepSetC[A, B, C]) =
    val v = VariantSetIrregC(as, bs, cs, f)
    v.irregComprehension[Z](id)

  def apply[T[_]](as: => T[A], bs: => T[B], cs: => T[C])(using Mappable[T]) =
    val v = baseShape(as, bs, cs, f)
    v.rectComprehensionDerived[Z](id)

  def apply[T[_]](as: => Mixed[A, T], bs: => Mixed[B, T], cs: => Mixed[C, T], b: Boolean = true)(using
      m: Mappable[T],
      tagA: ClassTag[A],
      tagB: ClassTag[B],
      tagC: ClassTag[C]
  ) =
    if m.isDelayed then cr.rectDerived(as.toDerivedFromMixed(), toDerived(bs), toDerived(cs), f, id)
    else cr.rectDerived(toDerived(as), toDerived(bs), toDerived(cs), f, id)

  def applyD[T[_]](as: T[A], bs: T[B], cs: DepTB[T, B, C])(using Mappable[T]) =
    apply(as, bs, cs)

  def apply[T[_]](as: T[A], bs: T[B], cs: DepTB[T, B, C])(using Mappable[T]) =
    val v = baseShape(as, a => bs, cs, f)
    v.irregComprehensionDerived[Z](id)

  def applyC[T[_]](as: T[A], bs: T[B], cs: DepTC[T, A, B, C])(using Mappable[T]) =
    val v = baseShape(as, a => bs, cs, f)
    v.irregComprehensionDerived[Z](id)

  def applyAC[T[_]](as: T[A], bs: DepTB[T, A, B], cs: DepTC[T, A, B, C])(using Mappable[T]) =
    apply(as, bs, cs)

  def apply[T[_]](as: T[A], bs: DepTB[T, A, B], cs: DepTC[T, A, B, C])(using Mappable[T]) =
    val v = baseShape(as, bs, cs, f)
    v.irregComprehensionDerived[Z](id)

  def applyT[T[_]](as: T[A], bs: T[B], cs: T[C])(using Mappable[T]) =
    val v = baseShape(as, bs, cs, f)
    v.rectComprehensionDerived[Z](id)

  def apply(as: MappableT[A], b: B, c: C) = as.getFnValueC1(f, b, c)
  def apply(a: A, bs: MappableT[B], c: C) = bs.getFnValueC2(f, a, c)
  def apply(a: A, b: B, cs: MappableT[C]) = cs.getFnValueC3(f, a, b)

  def applyV[T[_]: Mappable, U[_]: Mappable, V[_]: Mappable](as: => T[A], bs: => U[B], cs: => V[C]): T[U[V[Z]]] =
    val y = VariantRectDerivedMultipleC[T, U, V, Z, A, B, C, R, S](as, bs, cs, f)
    y.rectComprehensionDerivedMultiple()

  def applyTU[T[_]: Mappable, U[_]: Mappable](as: => T[U[A]], bs: => T[U[B]], cs: => T[U[C]]): T[U[Z]] =
    as.flatMap: a =>
      bs.flatMap: b =>
        cs.map: c =>
          augment(f)(a, b, c)

  def apply(as: JList[A], bs: JList[B], cs: JList[C]): R[Z, A, B, C] =
    val (as1, bs1, cs1) = (as.asScala.toList, bs.asScala.toList, cs.asScala.toList)
    given ClassTag[Z] = ClassTag(f(as1.head, bs1.head, cs1.head).getClass)
    val v = baseShape(as1, bs1, cs1, f)
    v.rectComprehension(id)

  def applyJ(
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

  def crossCheck(as: Seq[A], bs: GenSeqB[A, B], cs: GenSeqC[A, B, C], r: Seq[Z]) =
    val vf = AugmentC[SeqC, SeqC]()(as, bs, cs, f)
    val (rf, tf) = timed(vf.irregComprehension[Z](id))
    displayCheckRes(r, rf, 0, tf, true)
    assert(r == rf)
    (r, rf)

  def crossCheck(as: Seq[A], bs: Seq[B], cs: Seq[C], g: (Seq[A], Seq[B], Seq[C]) => Seq[Z]): (Seq[Z], Seq[Z]) =
    crossCheck(g, as, bs, cs)

  def crossCheck(g: (Seq[A], Seq[B], Seq[C]) => Seq[Z], as: Seq[A], bs: Seq[B], cs: Seq[C]) =
    val (r, t) = timed(g(as, bs, cs))
    val vf = AugmentC[SeqC, SeqC]()(as, bs, cs, f)
    val (rf, tf) = timed(vf.irregComprehension[Z](id))
    displayCheckRes(r, rf, t, tf, true)
    assert(r == rf)
    (r, rf)

  def crossCheckFX[T[_]](as: => Mixed[A, T], bs: => Mixed[B, T], cs: => Mixed[C, T], r: T[Z])(using
      Mappable[T],
      ClassTag[A],
      ClassTag[B],
      ClassTag[C]
  ): (T[Z], T[Z]) =
    val (rf, tf) = timed(apply(as, bs, cs))
    assert(r == rf)
    (r, rf)

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

  def apply[E, F](es: Seq[E], fs: Seq[F], g: (E, F) => (A, B, C, D))(using tagZ: ClassTag[Z]) =
    val v = AugmentB[MultiArrayB, SeqB]()(es, fs, g)
    v.rectComprehension(f(_, _, _, _))

  def applyS(as: Set[A], bs: DepSetB[A, B], cs: DepSetC[A, B, C], ds: DepSetD[A, B, C, D]) =
    val v = VariantSetIrregD(as, bs, cs, ds, f)
    v.irregComprehension[Z](id)

  def applyCD[T[_]](as: T[A], bs: DepTB[T, A, B], cs: DepTB[T, B, C], ds: DepTB[T, C, D])(using Mappable[T]) =
    val v = baseShape(as, bs, cs, ds, f)
    v.irregComprehensionDerived[Z](id)

  def applyD[T[_]](as: T[A], bs: DepTB[T, A, B], cs: DepTC[T, A, B, C], ds: DepTD[T, A, B, C, D])(using Mappable[T]) =
    val v = baseShape(as, bs, cs, ds, f)
    v.irregComprehensionDerived[Z](id)

  def applyV[T[_]](as: T[A], bs: T[B], cs: (A, B) => C, ds: C => T[D])(using t: Mappable[T]) =
    applyD(as, _ => bs, cs(_, _).unit(), (a, b, c) => ds(c))

  def applyT[T[_]](as: T[A], bs: T[B], cs: T[C], ds: T[D])(using Mappable[T]) =
    val v = baseShape(as, bs, cs, ds, f)
    v.rectComprehensionDerived[Z](id)

  def applyMX[T[_]](
      as: => Mixed[A, T],
      bs: A => Mixed[B, T],
      cs: B => Mixed[C, T],
      ds: C => Mixed[D, T],
      z: Boolean = true
  )(using
      m: Mappable[T],
      tagA: ClassTag[A],
      tagB: ClassTag[B],
      tagC: ClassTag[C],
      tagD: ClassTag[D]
  ): T[Z] =
    if m.isDelayed then
      cy.irregularDerived(
        as.toDerivedFromMixed(),
        a => toDerived(bs(a)),
        (a, b) => toDerived(cs(b)),
        (a, b, c) => toDerived(ds(c)),
        f,
        id
      )
    else
      cy.irregularDerived(
        toDerived(as),
        a => toDerived(bs(a)),
        (a, b) => toDerived(cs(b)),
        (a, b, c) => toDerived(ds(c)),
        f,
        id
      )

  def apply(as: JList[A], bs: JList[B], cs: JList[C], ds: JList[D]): MultiArrayD[Z, A, B, C, D] =
    val (as1, bs1, cs1, ds1) = (as.asScala.toList, bs.asScala.toList, cs.asScala.toList, ds.asScala.toList)
    given ClassTag[Z] = ClassTag(f(as1.head, bs1.head, cs1.head, ds1.head).getClass)
    val v = AugmentD[MultiArrayD, SeqD]()(as1, bs1, cs1, ds1, f)
    v.rectComprehension(id)

trait AugmentedFnE[Z, A, B, C, D, E, R[_, _, _, _, _, _], S[_, _, _, _, _, _]](using
    cr: ComprehensionE[R],
    cs: ComprehensionE[S]
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

  def applyT[T[_]](as: T[A], bs: T[B], cs: T[C], ds: T[D], es: T[E])(using Mappable[T]) =
    val v = baseShape(as, bs, cs, ds, es, f)
    v.rectComprehensionDerived[Z](id)

  def applyTD[T[_]](as: T[A], bs: DepTB[T, A, B], cs: DepTB[T, B, C], ds: DepTB[T, C, D], es: DepTB[T, D, E])(using
      Mappable[T]
  ) =
    val v = baseShape(as, bs, cs, ds, es, f)
    v.irregComprehensionDerived[Z](id)

  def applyTE[T[_]](
      as: T[A],
      bs: DepTB[T, A, B],
      cs: DepTC[T, A, B, C],
      ds: DepTD[T, A, B, C, D],
      es: DepTE[T, A, B, C, D, E]
  )(using
      Mappable[T]
  ) =
    val v = baseShape apply (as, bs, cs, ds, es, f)
    v.irregComprehensionDerived[Z](id)

  def apply[F, G](fs: Seq[F], gs: Seq[G], g: (F, G) => (A, B, C, D, E))(using tagZ: ClassTag[Z]) =
    val v = AugmentB[MultiArrayB, SeqB]()(fs, gs, g)
    v.rectComprehension(f(_, _, _, _, _))

  def apply(as: JList[A], bs: JList[B], cs: JList[C], ds: JList[D], es: JList[E])(using ClassTag[Z]) =
    val (as1, bs1, cs1, ds1, es1) =
      (as.asScala.toList, bs.asScala.toList, cs.asScala.toList, ds.asScala.toList, es.asScala.toList)
    val v = baseShape(as1, bs1, cs1, ds1, es1, f)
    v.rectComprehension[Z](id)

  def apply(as: JList[A], bs: JList[B], cs: JList[C], ds: JList[D], es: JList[E]) =
    val (as1, bs1, cs1, ds1, es1) =
      (as.asScala.toList, bs.asScala.toList, cs.asScala.toList, ds.asScala.toList, es.asScala.toList)
    given ClassTag[Z] = ClassTag(f(as1.head, bs1.head, cs1.head, ds1.head, es1.head).getClass)
    val v = baseShape(as1, bs1, cs1, ds1, es1, f)
    v.rectComprehension(id)
