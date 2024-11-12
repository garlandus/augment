package augmented

/** Augmented functions: extending existing functions to handle additional argument types, particularly to reproduce
  * comprehensions.
  */

import basicdef._
import comprehension._
import comprehension.given
import mappable._
import mappable.given
import multiarray._
import shape._
import util._
import variant._

import scala.reflect.ClassTag
import scala.annotation.targetName

given AugmentA[ArrayNA, SeqA] = AugmentA()
given AugmentB[MultiArrayB, SeqB] = AugmentB()
given AugmentC[MultiArrayC, SeqC] = AugmentC()
given AugmentD[MultiArrayD, SeqD] = AugmentD()
given AugmentE[MultiArrayE, SeqE] = AugmentE()
given AugmentF[MultiArrayF, SeqF] = AugmentF()
given AugmentG[MultiArrayG, SeqG] = AugmentG()

given Effects[BasicIO] = Effects()

object augment:

  /** The main way to augment a function, using the type of containers that have been specified
   *  by a "given" statement (see above for the defaults).
   * 
   *  For instance
   *    given AugmentB[MultiArrayB, SeqB] = AugmentB()
   *  states that in the rectangular case, a comprehension will return a 2D array, whereas in the
   *  more general case, it will return a sequence: essentially a hybrid of an array comprehension
   *  and a list comprehension.
   * 
   *  Z is the element type, and R and S are the higher-kinded container types returned in the
   *  rectangular and irregular cases respectively.
   *  The original argument types (A, B, C, ...) are retained in the case of an array (for the axes),
   *  but are ignored elsewhere.
   */
  def apply[Z, A, R[_, _], S[_, _]]                                     (f: A => Z)               (using aug: AugmentA[R, S]) = aug (f)
  def apply[Z, A, B, R[_, _, _], S[_, _, _]]                            (f: (A, B) => Z)          (using aug: AugmentB[R, S]) = aug (f)
  def apply[Z, A, B, C, R[_, _, _, _], S[_, _, _, _]]                   (f: (A, B, C) => Z)       (using aug: AugmentC[R, S]) = aug (f)
  def apply[Z, A, B, C, D, R[_, _, _, _, _], S[_, _, _, _, _]]          (f: (A, B, C, D) => Z)    (using aug: AugmentD[R, S]) = aug (f)
  def apply[Z, A, B, C, D, E, R[_, _, _, _, _, _], S[_, _, _, _, _, _]] (f: (A, B, C, D, E) => Z) (using aug: AugmentE[R, S]) = aug (f)
  def apply[Z, A, B, C, D, E, F, R[_, _, _, _, _, _, _], S[_, _, _, _, _, _, _]] (f: (A, B, C, D, E, F) => Z)
    (using aug: AugmentF[R, S]) = aug (f)
  def apply[Z, A, B, C, D, E, F, G, R[_, _, _, _, _, _, _, _], S[_, _, _, _, _, _, _, _]]
    (f: (A, B, C, D, E, F, G) => Z) (using aug: AugmentG[R, S]) = aug (f)

  def apply[A, R[_, _], S[_, _]]                                        (f: A => Boolean)         (using aug: AugmentA[R, S]) = aug (f)
  def apply[T[_]: Applicative, Z, A, R[_, _], S[_, _]]                  (f: A => T[Z])            (using aug: AugmentA[R, S]) = aug (f)
  def apply[A, R[_, _, _], S[_, _, _]]                                  (f: (A, A) => A)          (using aug: AugmentB[R, S]) = aug (f)
  def applyA[T[_]: Applicative, Z, A, B, R[_, _, _], S[_, _, _]]        (f: (A, B) => T[Z])       (using aug: AugmentB[R, S]) = aug (f)
  def applyA[T[_]: Applicative, Z, A, B, C, R[_, _, _, _],S[_, _, _, _]](f: (A, B, C) => T[Z])    (using aug: AugmentC[R, S]) = aug (f)

  /** The original list comprehension: always return a sequence */
  infix def flat[Z, A]                  (f: A => Z)                     = AugmentA[SeqA, SeqA]() (f)
  infix def flat[Z, A, B]               (f: (A, B) => Z)                = AugmentB[SeqB, SeqB]() (f)
  infix def flat[Z, A, B, C]            (f: (A, B, C) => Z)             = AugmentC[SeqC, SeqC]() (f)
  infix def flat[Z, A, B, C, D]         (f: (A, B, C, D) => Z)          = AugmentD[SeqD, SeqD]() (f)
  infix def flat[Z, A, B, C, D, E]      (f: (A, B, C, D, E) => Z)       = AugmentE[SeqE, SeqE]() (f)
  infix def flat[Z, A, B, C, D, E, F]   (f: (A, B, C, D, E, F) => Z)    = AugmentF[SeqF, SeqF]() (f)
  infix def flat[Z, A, B, C, D, E, F, G](f: (A, B, C, D, E, F, G) => Z) = AugmentG[SeqG, SeqG]() (f)

  /** Specifies that comprehensions should return nested sequences
   *  For instance Pascal's triangle can be given by a comprehension with
   *    val binomialCoefficient = augment seq (CombinatoricsUtils.binomialCoefficient)
   */
  infix def seq[Z, A]                   (f: A => Z)                     = AugmentA[SeqNA, SeqNA]() (f)
  infix def seq[Z, A, B]                (f: (A, B) => Z)                = AugmentB[SeqNB, SeqNB]() (f)
  infix def seq[Z, A, B, C]             (f: (A, B, C) => Z)             = AugmentC[SeqNC, SeqNC]() (f)
  infix def seq[Z, A, B, C, D]          (f: (A, B, C, D) => Z)          = AugmentD[SeqND, SeqND]() (f)
  infix def seq[Z, A, B, C, D, E]       (f: (A, B, C, D, E) => Z)       = AugmentE[SeqNE, SeqNE]() (f)
  infix def seq[Z, A, B, C, D, E, F]    (f: (A, B, C, D, E, F) => Z)    = AugmentF[SeqNF, SeqNF]() (f)
  infix def seq[Z, A, B, C, D, E, F, G] (f: (A, B, C, D, E, F, G) => Z) = AugmentG[SeqNG, SeqNG]() (f)

  /** Return maps in the rectangular case and sequences otherwise */
  infix def mapped[Z, A]                (f: A => Z)                     = AugmentA[MapA, SeqA]() (f)
  infix def mapped[Z, A, B]             (f: (A, B) => Z)                = AugmentB[MapB, SeqB]() (f)
  infix def mapped[Z, A, B, C]          (f: (A, B, C) => Z)             = AugmentC[MapC, SeqC]() (f)
  infix def mapped[Z, A, B, C, D]       (f: (A, B, C, D) => Z)          = AugmentD[MapD, SeqD]() (f)
  infix def mapped[Z, A, B, C, D, E]    (f: (A, B, C, D, E) => Z)       = AugmentE[MapE, SeqE]() (f)

  /** Return sets */
  infix def set[Z, A]                   (f: A => Z)                     = AugmentA[SetA, SetA]() (f)
  infix def set[Z, A, B]                (f: (A, B) => Z)                = AugmentB[SetB, SetB]() (f)
  infix def set[Z, A, B, C]             (f: (A, B, C) => Z)             = AugmentC[SetC, SetC]() (f)
  infix def set[Z, A, B, C, D]          (f: (A, B, C, D) => Z)          = AugmentD[SetD, SetD]() (f)
  infix def set[Z, A, B, C, D, E]       (f: (A, B, C, D, E) => Z)       = AugmentE[SetE, SetE]() (f)

  /** Allows you to specify pre- and post-conditions */
  def apply[Z, A]             (f: A => Z, c: Conditions[Z, A])                            = AugmentA[ArrayNA, SeqA]()(f, c)
  def apply[Z, A, B]          (f: (A, B) => Z, c: Conditions[Z, (A, B)])                  = AugmentB[MultiArrayB, SeqB]()(f, c)
  def apply[Z, A, B, C]       (f: (A, B, C) => Z, c: Conditions[Z, (A, B, C)])            = AugmentC[MultiArrayC, SeqC]()(f, c)
  def apply[Z, A, B, C, D]    (f: (A, B, C, D) => Z, c: Conditions[Z, (A, B, C, D)])      = AugmentD[MultiArrayD, SeqD]()(f, c)
  def apply[Z, A, B, C, D, E] (f: (A, B, C, D, E) => Z, c: Conditions[Z, (A, B, C, D, E)])= AugmentE[MultiArrayE, SeqE]()(f, c)

  def apply[Z, A]             (f: A => Z, preCond: Condition[A], postCond: Condition[(Z, A)]) =
    AugmentA[ArrayNA, SeqA]()(f, Conditions(preCond, postCond))

/** Usually an existing function is first augmented, then applied: by contrast here the base lambda function is provided
  * along with its arguments. Although it might be preferable to have it as the first argument, type inference currently
  * works better when the lambda function is the last argument.
  */
object image:
  def apply[Z, A](as: Seq[A], f: A => Z)(using ClassTag[Z]) =
    augment(f)(as)
  def apply[Z, A](as: Seq[A], phi: A => Boolean, f: A => Z)(using ClassTag[Z]) =
    augment(f)(as, phi)
  def apply[T[_]: Mappable, Z, A](as: T[A], f: A => Z) =
    augment(f)(as)

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
  def apply[T[_]: Mappable, Z, A, B](as: Mixed[A, T], bs: Mixed[B, T], f: (A, B) => Z)(using
      ClassTag[A],
      ClassTag[B],
      ClassTag[Z]
  ) =
    lastC applyMixed (as, _ => bs, f)

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

  def apply[T[_]: Mappable, Z, A, B, C](as: T[A], bs: DepTB[T, A, B], cs: DepTC[T, A, B, C], f: (A, B, C) => Z) =
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
    augment(f) applyRectangular (as, bs)

  def apply[T[_]: Mappable, Z, A, B, C](as: T[A], bs: T[B], cs: T[C], f: (A, B, C) => Z)(using ClassTag[Z]) =
    augment(f) applyRectangular (as, bs, cs)

case class Effects[T[_]: Mappable]():
  def apply() = ()

/** This simplfies a common case: a base function that simply returns a tuple of its arguments */
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
    tupleC.applySet(as, bs, (_, b) => cs(b))

  def apply[T[_]: Mappable, A, B, C](as: T[A], bs: T[B], cs: T[C]) =
    tupleC applyRectangular (as, bs, cs)

object count:
  def apply[A, B, C](as: Seq[A], bs: DepSeqB[A, B], cs: DepSeqB[B, C]) =
    image(tupledC, as, bs, (_, b) => cs(b)).length

object queue:

  def apply[T[_], A, B, C](a: => A, b: => B, cs: B => C, z: Boolean = true)(using fx: Effects[T], t: Mappable[T]) =
    lastC applyPlainForm (a, _ => b, (_, b) => cs(b))

  def apply[T[_], A, B, C](a: => A, bs: A => B, cs: B => C)(using fx: Effects[T], t: Mappable[T]) =
    lastC applyPlainForm (a, bs, (_, b) => cs(b))

  def apply[T[_], A, B, C](a: => A, b: => B, cs: (A, B) => C)(using fx: Effects[T], t: Mappable[T]) =
    lastC applyPlainForm (a, _ => b, cs)

  def apply[T[_], A, B, C](a: => A, bs: A => B, c: C)(using fx: Effects[T], t: Mappable[T]) =
    lastC applyPlainForm (a, bs, (_, _) => c)

  def apply[T[_], A, B, C, D](a: => A, bs: A => B, cs: (A, B) => C, ds: (A, B, C) => D)(using
      fx: Effects[T],
      t: Mappable[T]
  ) =
    lastD applyPlainForm (a, bs, cs, ds)

  def apply[T[_], A, B, C, D](a: => A, b: => B, cs: (A, B) => C, ds: C => D)(using fx: Effects[T], t: Mappable[T]) =
    lastD applyPlainForm (a, _ => b, cs, (_, _, c) => ds(c))

  def apply[T[_], A, B, C, D](a: => A, bs: A => B, cs: B => C, ds: C => D)(using fx: Effects[T], t: Mappable[T]) =
    lastD applyPlainForm (a, bs, (_, b) => cs(b), (_, _, c) => ds(c))

  def apply[T[_], A, B, C, D](a: => A, b: => B, cs: B => C, ds: B => D)(using fx: Effects[T], t: Mappable[T]) =
    lastD applyPlainForm (a, _ => b, (_, b) => cs(b), (_, b, _) => ds(b))

  def apply[T[_], A, B, C, D](a: => A, b: => B, c: => C, ds: (A, C) => D)(using fx: Effects[T], t: Mappable[T]) =
    lastD applyPlainForm (a, _ => b, (_, _) => c, (a, _, c) => ds(a, c))

  @targetName("queueD")
  def apply[T[_], A, B, C, D](a: => A, bs: A => B, cs: B => C, ds: C => D)(using fx: Effects[T], t: Mappable[T]) =
    lastD applyPlainForm (a, bs, (_, b) => cs(b), (_, _, c) => ds(c))

  def apply[T[_], A, B, C, D, E](a: => A, bs: A => B, cs: B => C, ds: C => D, es: D => E)(using
      fx: Effects[T],
      t: Mappable[T]
  ) =
    lastE applyPlainForm (a, bs, (_, b) => cs(b), (_, _, c) => ds(c), (_, _, _, d) => es(d))

def tupledB[A, B]         = (a: A, b: B) => (a, b)
def tupledC[A, B, C]      = (a: A, b: B, c: C) => (a, b, c)
def tupleB[A, B]          = augment((a: A, b: B) => (a, b))
def tupleC[A, B, C]       = augment((a: A, b: B, c: C) => (a, b, c))
def tupleD[A, B, C, D]    = augment((a: A, b: B, c: C, d: D) => (a, b, c, d))
def tupleE[A, B, C, D, E] = augment((a: A, b: B, c: C, d: D, e: E) => (a, b, c, d, e))

def lastB[A, B]                 = augment((a: A, b: B) => b)
def lastC[A, B, C]              = augment((a: A, b: B, c: C) => c)
def lastD[A, B, C, D]           = augment((a: A, b: B, c: C, d: D) => d)
def lastE[A, B, C, D, E]        = augment((a: A, b: B, c: C, d: D, e: E) => e)
def lastF[A, B, C, D, E, F]     = augment((a: A, b: B, c: C, d: D, e: E, f: F) => f)
def lastG[A, B, C, D, E, F, G]  = augment((a: A, b: B, c: C, d: D, e: E, f: F, g: G) => g)

case class AugmentA[R[_, _], S[_, _]]()(using cx: ComprehensionA[R], cy: ComprehensionA[S]):
  def apply[Z, A](f: A => Z)                                        = AugmentedFunctionA[Z, A, R, S](f)

  def apply[Z, A](f: A => Z, c: Conditions[Z, A])                   = AugmentedFunctionA[Z, A, R, S](c.exec (f, _))
  def apply[Z, A](as: Seq[A], f: A => Z)                            = VariantRectA[Z, A, R, S](as, f)
  def apply[Z, A](as: Seq[A], phi: A => Boolean, f: A => Z)         = VariantIrregA[Z, A, R, S](as, phi, f)

  def apply[T[_]: Applicative, Z, A](f: A => T[Z])                  = AugmentedFunctionApplicA[T, Z, A, R, S](f)
  def apply[T[_]: Mappable, Z, A](as: => T[A], f: A => Z)           = VariantRectDerivedA[T, Z, A, R, S](as, f)
  def apply[T[_]: Mappable, U[_]: Mappable, Z, A](f: A => T[U[Z]])  = AugmentedFunctionTUA[T, U, Z, A, R, S](f)

  def apply[A](f: A => Boolean)                                     = AugmentedFunctionPredA[A, R, S](f)

case class AugmentB[R[_, _, _], S[_, _, _]]()(using cx: ComprehensionB[R], cy: ComprehensionB[S]):
  def apply[Z, A, B](f: (A, B) => Z)                                = AugmentedFunctionB[Z, A, B, R, S](f)

  def apply[Z, A, B](f: (A, B) => Z, cond: Conditions[Z, (A, B)]) =
    AugmentedFunctionB[Z, A, B, R, S]((a, b) => cond.exec(f.tupled, (a, b)))
  def apply[Z, A, B](as: Seq[A], bs: Seq[B], f: (A, B) => Z)          = VariantRectB[Z, A, B, R, S](as, bs, f)
  def apply[Z, A, B](as: Seq[A], bsGen: GenSeqB[A, B], f: (A, B) => Z)= VariantIrregB[Z, A, B, R, S](as, seq(bsGen), f)

  def apply[T[_]: Applicative, Z, A, B](f: (A, B) => T[Z])          = AugmentedFunctionApplicB[T, Z, A, B, R, S](f)
  def apply[T[_]: Mappable, Z, A, B](as: => T[A], bs: => T[B], f: (A, B) => Z) =
    VariantRectDerivedB[T, Z, A, B, R, S](as, bs, f)

  def apply[A](f: (A, A) => A)                                      = AugmentedFunctionSingleTypeB[A, R, S](f)

case class AugmentC[R[_, _, _, _], S[_, _, _, _]]()(using cx: ComprehensionC[R], cy: ComprehensionC[S]):
  def apply[Z, A, B, C](f: (A, B, C) => Z) = AugmentedFunctionC[Z, A, B, C, R, S](f)
  def apply[T[_]: Applicative, Z, A, B, C](f: (A, B, C) => T[Z]) = AugmentedFunctionApplicC[T, Z, A, B, C, R, S](f)

  def apply[Z, A, B, C](f: (A, B, C) => Z, cond: Conditions[Z, (A, B, C)]) =
    AugmentedFunctionC[Z, A, B, C, R, S]((a, b, c) => cond.exec(f.tupled, (a, b, c)))
  def apply[Z, A, B, C](as: Seq[A], bs: Seq[B], cs: Seq[C], f: (A, B, C) => Z) =
    VariantRectC[Z, A, B, C, R, S](as, bs, cs, f)
  def apply[Z, A, B, C](as: Seq[A], bsGen: GenSeqB[A, B], csGen: GenSeqC[A, B, C], f: (A, B, C) => Z) =
    VariantIrregC[Z, A, B, C, R, S](as, seq(bsGen), seq(csGen), f)
  def apply[Z, A, B, C](as: Seq[A], bsGen: DepSeqB[A, B], csGen: DepSeqC[A, B, C], f: (A, B, C) => Z) =
    VariantIrregC[Z, A, B, C, R, S](as, bsGen, csGen, f)

  def apply[T[_]: Mappable, Z, A, B, C](as: => T[A], bs: => T[B], cs: => T[C], f: (A, B, C) => Z) =
    VariantRectDerivedC[T, Z, A, B, C, R, S](as, bs, cs, f)

  def apply[T[_]: Mappable, Z, A, B, C](as: T[A], bsDep: DepTB[T, A, B], csDep: DepTC[T, A, B, C], f: (A, B, C) => Z) =
    VariantIrregDerivedC[T, Z, A, B, C, R, S](as, bsDep, csDep, f)

  def apply[T[_]: Mappable, Z, A, B, C](as: T[A], bs: T[B], csDep: DepTB[T, B, C], f: (A, B, C) => Z) =
    VariantIrregDerivedC[T, Z, A, B, C, R, S](as, a => bs, (a, b) => csDep(b), f)

  def apply[T[_]: Mappable, Z, A, B, C](as: T[A], bsDep: DepTB[T, A, B], csDep: DepTB[T, B, C], f: (A, B, C) => Z) =
    VariantIrregDerivedC[T, Z, A, B, C, R, S](as, bsDep, (a, b) => csDep(b), f)

case class AugmentD[R[_, _, _, _, _], S[_, _, _, _, _]]()(using cx: ComprehensionD[R], cy: ComprehensionD[S]):
  def apply[Z, A, B, C, D](f: (A, B, C, D) => Z) = AugmentedFunctionD[Z, A, B, C, D, R, S](f)
  def apply[Z, A, B, C, D](f: (A, B, C, D) => Z, cond: Conditions[Z, (A, B, C, D)]) =
    AugmentedFunctionD[Z, A, B, C, D, R, S]((a, b, c, d) => cond.exec(f.tupled, (a, b, c, d)))
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

  def apply[T[_]: Mappable, Z, A, B, C, D](
      as: T[A],
      bsDep: DepTB[T, A, B],
      csDep: DepTB[T, B, C],
      dsDep: DepTB[T, C, D],
      f: (A, B, C, D) => Z
  ) =
    VariantIrregDerivedD[T, Z, A, B, C, D, R, S](as, bsDep, (a, b) => csDep(b), (a, b, c) => dsDep(c), f)

  def apply[T[_]: Mappable, Z, A, B, C, D](
      as: T[A],
      bsDep: DepTB[T, A, B],
      csDep: DepTC[T, A, B, C],
      dsDep: DepTD[T, A, B, C, D],
      f: (A, B, C, D) => Z
  ) =
    VariantIrregDerivedD[T, Z, A, B, C, D, R, S](as, bsDep, csDep, dsDep, f)

  def apply[T[_]: Mappable, Z, A, B, C, D](as: => T[A], bs: => T[B], cs: => T[C], ds: => T[D], f: (A, B, C, D) => Z) =
    VariantRectDerivedD[T, Z, A, B, C, D, R, S](as, bs, cs, ds, f)

case class AugmentE[R[_, _, _, _, _, _], S[_, _, _, _, _, _]]()(using cx: ComprehensionE[R], cy: ComprehensionE[S]):
  def apply[Z, A, B, C, D, E](f: (A, B, C, D, E) => Z) = AugmentedFunctionE[Z, A, B, C, D, E, R, S](f)
  def apply[Z, A, B, C, D, E](f: (A, B, C, D, E) => Z, cond: Conditions[Z, (A, B, C, D, E)]) =
    AugmentedFunctionE[Z, A, B, C, D, E, R, S]((a, b, c, d, e) => cond.exec(f.tupled, (a, b, c, d, e)))
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

  def apply[T[_]: Mappable, Z, A, B, C, D, E](
      as: => T[A],
      bs: => T[B],
      cs: => T[C],
      ds: => T[D],
      es: => T[E],
      f: (A, B, C, D, E) => Z
  ) =
    VariantRectDerivedE[T, Z, A, B, C, D, E, R, S](as, bs, cs, ds, es, f)

  def apply[T[_]: Mappable, Z, A, B, C, D, E](
      as: T[A],
      bsDep: DepTB[T, A, B],
      csDep: DepTB[T, B, C],
      dsDep: DepTB[T, C, D],
      esDep: DepTB[T, D, E],
      f: (A, B, C, D, E) => Z
  ) =
    VariantIrregDerivedE[T, Z, A, B, C, D, E, R, S](
      as,
      bsDep,
      (a, b) => csDep(b),
      (a, b, c) => dsDep(c),
      (a, b, c, d) => esDep(d),
      f
    )

  def apply[T[_]: Mappable, Z, A, B, C, D, E](
      as: T[A],
      bsDep: DepTB[T, A, B],
      csDep: DepTC[T, A, B, C],
      dsDep: DepTD[T, A, B, C, D],
      esDep: DepTE[T, A, B, C, D, E],
      f: (A, B, C, D, E) => Z
  ) =
    VariantIrregDerivedE[T, Z, A, B, C, D, E, R, S](
      as,
      bsDep,
      (a, b) => csDep(a, b),
      (a, b, c) => dsDep(a, b, c),
      (a, b, c, d) => esDep(a, b, c, d),
      f
    )

case class AugmentF[R[_, _, _, _, _, _, _], S[_, _, _, _, _, _, _]]()(using
    cx: ComprehensionF[R],
    cy: ComprehensionF[S]
):
  def apply[Z, A, B, C, D, E, F](f: (A, B, C, D, E, F) => Z) = AugmentedFunctionF[Z, A, B, C, D, E, F, R, S](f)
  def apply[Z, A, B, C, D, E, F](f: (A, B, C, D, E, F) => Z, cond: Conditions[Z, (A, B, C, D, E, F)]) =
    AugmentedFunctionF[Z, A, B, C, D, E, F, R, S]((a, b, c, d, e, f0) => cond.exec(f.tupled, (a, b, c, d, e, f0)))

  def apply[Z, A, B, C, D, E, F](
      as: Seq[A],
      bs: Seq[B],
      cs: Seq[C],
      ds: Seq[D],
      es: Seq[E],
      fs: Seq[F],
      f: (A, B, C, D, E, F) => Z
  ) =
    VariantRectF[Z, A, B, C, D, E, F, R, S](as, bs, cs, ds, es, fs, f)

  def apply[Z, A, B, C, D, E, F](
      as: Seq[A],
      bsGen: DepSeqB[A, B],
      csGen: DepSeqC[A, B, C],
      dsGen: DepSeqD[A, B, C, D],
      esGen: DepSeqE[A, B, C, D, E],
      fsGen: DepSeqF[A, B, C, D, E, F],
      f: (A, B, C, D, E, F) => Z
  ) =
    VariantIrregF[Z, A, B, C, D, E, F, R, S](as, bsGen, csGen, dsGen, esGen, fsGen, f)

  def apply[T[_]: Mappable, Z, A, B, C, D, E, F](
      as: => T[A],
      bs: => T[B],
      cs: => T[C],
      ds: => T[D],
      es: => T[E],
      fs: => T[F],
      f: (A, B, C, D, E, F) => Z
  ) =
    VariantRectDerivedF[T, Z, A, B, C, D, E, F, R, S](as, bs, cs, ds, es, fs, f)

  def apply[T[_]: Mappable, Z, A, B, C, D, E, F](
      as: T[A],
      bsDep: DepTB[T, A, B],
      csDep: DepTB[T, B, C],
      dsDep: DepTB[T, C, D],
      esDep: DepTB[T, D, E],
      fsDep: DepTB[T, E, F],
      f: (A, B, C, D, E, F) => Z
  ) =
    VariantIrregDerivedF[T, Z, A, B, C, D, E, F, R, S](
      as,
      bsDep,
      (a, b) => csDep(b),
      (a, b, c) => dsDep(c),
      (a, b, c, d) => esDep(d),
      (a, b, c, d, e) => fsDep(e),
      f
    )

  def apply[T[_]: Mappable, Z, A, B, C, D, E, F](
      as: T[A],
      bsDep: DepTB[T, A, B],
      csDep: DepTC[T, A, B, C],
      dsDep: DepTD[T, A, B, C, D],
      esDep: DepTE[T, A, B, C, D, E],
      fsDep: DepTF[T, A, B, C, D, E, F],
      f: (A, B, C, D, E, F) => Z
  ) =
    VariantIrregDerivedF[T, Z, A, B, C, D, E, F, R, S](
      as,
      bsDep,
      (a, b) => csDep(a, b),
      (a, b, c) => dsDep(a, b, c),
      (a, b, c, d) => esDep(a, b, c, d),
      (a, b, c, d, e) => fsDep(a, b, c, d, e),
      f
    )

case class AugmentG[R[_, _, _, _, _, _, _, _], S[_, _, _, _, _, _, _, _]]()(using
    cx: ComprehensionG[R],
    cy: ComprehensionG[S]
):
  def apply[Z, A, B, C, D, E, F, G](f: (A, B, C, D, E, F, G) => Z) = AugmentedFunctionG[Z, A, B, C, D, E, F, G, R, S](f)
  def apply[Z, A, B, C, D, E, F, G](f: (A, B, C, D, E, F, G) => Z, cond: Conditions[Z, (A, B, C, D, E, F, G)]) =
    AugmentedFunctionG[Z, A, B, C, D, E, F, G, R, S]((a, b, c, d, e, f0, g) =>
      cond.exec(f.tupled, (a, b, c, d, e, f0, g))
    )

  def apply[Z, A, B, C, D, E, F, G](
      as: Seq[A],
      bs: Seq[B],
      cs: Seq[C],
      ds: Seq[D],
      es: Seq[E],
      fs: Seq[F],
      gs: Seq[G],
      f: (A, B, C, D, E, F, G) => Z
  ) =
    VariantRectG[Z, A, B, C, D, E, F, G, R, S](as, bs, cs, ds, es, fs, gs, f)

  def apply[Z, A, B, C, D, E, F, G](
      as: Seq[A],
      bsGen: DepSeqB[A, B],
      csGen: DepSeqC[A, B, C],
      dsGen: DepSeqD[A, B, C, D],
      esGen: DepSeqE[A, B, C, D, E],
      fsGen: DepSeqF[A, B, C, D, E, F],
      gsGen: DepSeqG[A, B, C, D, E, F, G],
      f: (A, B, C, D, E, F, G) => Z
  ) =
    VariantIrregG[Z, A, B, C, D, E, F, G, R, S](as, bsGen, csGen, dsGen, esGen, fsGen, gsGen, f)

  def apply[T[_]: Mappable, Z, A, B, C, D, E, F, G](
      as: => T[A],
      bs: => T[B],
      cs: => T[C],
      ds: => T[D],
      es: => T[E],
      fs: => T[F],
      gs: => T[G],
      f: (A, B, C, D, E, F, G) => Z
  ) =
    VariantRectDerivedG[T, Z, A, B, C, D, E, F, G, R, S](as, bs, cs, ds, es, fs, gs, f)

  def apply[T[_]: Mappable, Z, A, B, C, D, E, F, G](
      as: T[A],
      bsDep: DepTB[T, A, B],
      csDep: DepTB[T, B, C],
      dsDep: DepTB[T, C, D],
      esDep: DepTB[T, D, E],
      fsDep: DepTB[T, E, F],
      gsDep: DepTB[T, F, G],
      f: (A, B, C, D, E, F, G) => Z
  ) =
    VariantIrregDerivedG[T, Z, A, B, C, D, E, F, G, R, S](
      as,
      bsDep,
      (a, b) => csDep(b),
      (a, b, c) => dsDep(c),
      (a, b, c, d) => esDep(d),
      (a, b, c, d, e) => fsDep(e),
      (a, b, c, d, e, f0) => gsDep(f0),
      f
    )

  def apply[T[_]: Mappable, Z, A, B, C, D, E, F, G](
      as: T[A],
      bsDep: DepTB[T, A, B],
      csDep: DepTC[T, A, B, C],
      dsDep: DepTD[T, A, B, C, D],
      esDep: DepTE[T, A, B, C, D, E],
      fsDep: DepTF[T, A, B, C, D, E, F],
      gsDep: DepTG[T, A, B, C, D, E, F, G],
      f: (A, B, C, D, E, F, G) => Z
  ) =
    VariantIrregDerivedG[T, Z, A, B, C, D, E, F, G, R, S](
      as,
      bsDep,
      (a, b) => csDep(a, b),
      (a, b, c) => dsDep(a, b, c),
      (a, b, c, d) => esDep(a, b, c, d),
      (a, b, c, d, e) => fsDep(a, b, c, d, e),
      (a, b, c, d, e, f0) => gsDep(a, b, c, d, e, f0),
      f
    )

case class AugmentedFunctionA[Z, A, R[_, _], S[_, _]](f: A => Z)(using cx: ComprehensionA[R], cy: ComprehensionA[S])
    extends AugmentedFnA[Z, A, R, S]

case class AugmentedFunctionApplicA[T[_]: Applicative, Z, A, R[_, _], S[_, _]](f: A => T[Z])(using
    cx: ComprehensionA[R],
    cy: ComprehensionA[S]
) extends AugmentedFnApplicA[T, Z, A, R, S]

case class AugmentedFunctionTA[T[_]: Mappable, Z, A, R[_, _], S[_, _]](f: A => T[Z])(using
    cx: ComprehensionA[R],
    cy: ComprehensionA[S]
) extends AugmentedFnTA[T, Z, A, R, S]

case class AugmentedFunctionTUA[T[_]: Mappable, U[_]: Mappable, Z, A, R[_, _], S[_, _]](f: A => T[U[Z]])(using
    cx: ComprehensionA[R],
    cy: ComprehensionA[S]
) extends AugmentedFnTUA[T, U, Z, A, R, S]

case class AugmentedFunctionPredA[A, R[_, _], S[_, _]](f: A => Boolean)(using
    cx: ComprehensionA[R],
    cy: ComprehensionA[S]
) extends AugmentedFnPredA[A, R, S]

case class AugmentedFunctionB[Z, A, B, R[_, _, _], S[_, _, _]](f: (A, B) => Z)(using
    cx: ComprehensionB[R],
    cy: ComprehensionB[S]
) extends AugmentedFnB[Z, A, B, R, S]

case class AugmentedFunctionApplicB[T[_]: Applicative, Z, A, B, R[_, _, _], S[_, _, _]](f: (A, B) => T[Z])(using
    cx: ComprehensionB[R],
    cy: ComprehensionB[S]
) extends AugmentedFnApplicB[T, Z, A, B, R, S]

case class AugmentedFunctionSingleTypeB[A, R[_, _, _], S[_, _, _]](f: (A, A) => A)(using
    cx: ComprehensionB[R],
    cy: ComprehensionB[S]
) extends AugmentedFnB[A, A, A, R, S]
    with AugmentedFnSingleTypeB[A, R, S]

case class AugmentedFunctionC[Z, A, B, C, R[_, _, _, _], S[_, _, _, _]](f: (A, B, C) => Z)(using
    cx: ComprehensionC[R],
    cy: ComprehensionC[S]
) extends AugmentedFnC[Z, A, B, C, R, S]

case class AugmentedFunctionApplicC[T[_]: Applicative, Z, A, B, C, R[_, _, _, _], S[_, _, _, _]](f: (A, B, C) => T[Z])(
    using
    cx: ComprehensionC[R],
    cy: ComprehensionC[S]
) extends AugmentedFnApplicC[T, Z, A, B, C, R, S]

case class AugmentedFunctionD[Z, A, B, C, D, R[_, _, _, _, _], S[_, _, _, _, _]](f: (A, B, C, D) => Z)(using
    cx: ComprehensionD[R],
    cy: ComprehensionD[S]
) extends AugmentedFnD[Z, A, B, C, D, R, S]

case class AugmentedFunctionE[Z, A, B, C, D, E, R[_, _, _, _, _, _], S[_, _, _, _, _, _]](f: (A, B, C, D, E) => Z)(using
    cx: ComprehensionE[R],
    cy: ComprehensionE[S]
) extends AugmentedFnE[Z, A, B, C, D, E, R, S]

case class AugmentedFunctionF[Z, A, B, C, D, E, F, R[_, _, _, _, _, _, _], S[_, _, _, _, _, _, _]](
    f: (A, B, C, D, E, F) => Z
)(using
    cx: ComprehensionF[R],
    cy: ComprehensionF[S]
) extends AugmentedFnF[Z, A, B, C, D, E, F, R, S]

case class AugmentedFunctionG[Z, A, B, C, D, E, F, G, R[_, _, _, _, _, _, _, _], S[_, _, _, _, _, _, _, _]](
    f: (A, B, C, D, E, F, G) => Z
)(using
    cx: ComprehensionG[R],
    cy: ComprehensionG[S]
) extends AugmentedFnG[Z, A, B, C, D, E, F, G, R, S]

def id[A](a: A) = a

trait AugmentedFnABase[Z, A]:
  def f: A => Z
  def apply(a: A) =
    f(a)

trait AugmentedFnTABase[T[_], Z, A]:
  def f: A => T[Z]
  def apply(a: A) =
    f(a)

trait AugmentedFnApplicABase[T[_]: Applicative, Z, A]:
  def f: A => T[Z]
  def apply(a: A) =
    f(a)

trait AugmentedFnTUABase[T[_], U[_], Z, A]:
  def f: A => T[U[Z]]
  def apply(a: A) =
    f(a)

trait AugmentedFnPredBase[A]:
  def f: A => Boolean
  def apply(a: A) = f(a)

trait AugmentFnBBase[Z, A, B]:
  def f: (A, B) => Z
  def apply(a: A, b: B) =
    f(a, b)

trait AugmentedFnApplicBBase[T[_]: Applicative, Z, A, B]:
  def f: (A, B) => T[Z]
  def apply(a: A, b: B) =
    f(a, b)

trait AugmentFnSingleTypeBBase[A]:
  def f: (A, A) => A

trait AugmentedFnCBase[Z, A, B, C]:
  def f: (A, B, C) => Z
  def apply(a: A, b: B, c: C) =
    f(a, b, c)

trait AugmentedFnApplicCBase[T[_]: Applicative, Z, A, B, C]:
  def f: (A, B, C) => T[Z]
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

trait AugmentedFnFBase[Z, A, B, C, D, E, F]:
  def f: (A, B, C, D, E, F) => Z
  def apply(a: A, b: B, c: C, d: D, e: E, f0: F) =
    f(a, b, c, d, e, f0)

trait AugmentedFnGBase[Z, A, B, C, D, E, F, G]:
  def f: (A, B, C, D, E, F, G) => Z
  def apply(a: A, b: B, c: C, d: D, e: E, f0: F, g: G) =
    f(a, b, c, d, e, f0, g)
