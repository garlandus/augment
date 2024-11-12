package variant

import augmented._
import comprehension._
import comprehension.given
import mappable._
import multiarray._
import shape._

import scala.reflect.ClassTag
import scala.util.Random

trait VariantAT[X, A, R[_, _], S[_, _]]:
  def as: Seq[A]
  def f: A => X
  def rectComprehension[Z](using c: ComprehensionA[R], t: ClassTag[Z]): (X => Z) => R[Z, A]
  def irregComprehension[Z](using c: ComprehensionA[S]): (X => Z) => S[Z, A]

case class VariantRectA[X, A, R[_, _], S[_, _]](as: Seq[A], f: A => X) extends VariantAT[X, A, R, S]:
  def rectComprehension[Z](using c: ComprehensionA[R], t: ClassTag[Z]) = c.rectangular(as, f, _)
  def irregComprehension[Z](using c: ComprehensionA[S]) = ???

case class VariantIrregA[X, A, R[_, _], S[_, _]](as: Seq[A], phi: A => Boolean, f: A => X)
    extends VariantAT[X, A, R, S]:
  def rectComprehension[Z](using c: ComprehensionA[R], t: ClassTag[Z]) = ???
  def irregComprehension[Z](using c: ComprehensionA[S]) = c.irregular(as, phi, f, _)

case class VariantRectDerivedA[T[_]: Mappable, X, A, R[_, _], S[_, _]](as: T[A], f: A => X):
  def rectComprehensionDerived[Z](using c: ComprehensionA[S]): (X => Z) => T[Z] = c.rectDerived(as, f, _)

trait VariantBT[X, A, B, R[_, _, _], S[_, _, _]]:
  def as: Seq[A]
  def bsDep: DepSeqB[A, B]
  def f: (A, B) => X

  def rectComprehension[Z](using c: ComprehensionB[R], t: ClassTag[Z]): (X => Z) => R[Z, A, B]
  def irregComprehension[Z](using c: ComprehensionB[S]): (X => Z) => S[Z, A, B]

case class VariantRectB[X, A, B, R[_, _, _], S[_, _, _]](as: Seq[A], bs: Seq[B], f: (A, B) => X)
    extends VariantBT[X, A, B, R, S]:
  def bsDep = (a: A) => bs
  def rectComprehension[Z](using c: ComprehensionB[R], t: ClassTag[Z]) = c.rectangular(as, bs, f, _)
  def irregComprehension[Z](using c: ComprehensionB[S]) = c.irregular(as, bsDep, f, _)

case class VariantIrregB[X, A, B, R[_, _, _], S[_, _, _]](as: Seq[A], bsDep: DepSeqB[A, B], f: (A, B) => X)
    extends VariantBT[X, A, B, R, S]:
  def rectComprehension[Z](using c: ComprehensionB[R], t: ClassTag[Z]) = ???
  def irregComprehension[Z](using c: ComprehensionB[S]) = c.irregular(as, bsDep, f, _)

case class VariantRectDerivedB[T[_]: Mappable, X, A, B, R[_, _, _], S[_, _, _]](as: T[A], bs: T[B], f: (A, B) => X):
  def rectComprehensionDerived[Z](using c: ComprehensionB[S]): (X => Z) => T[Z] =
    c.rectDerived(as, bs, f, _)

case class VariantIrregDerivedB[T[_]: Mappable, X, A, B, R[_, _, _], S[_, _, _]](
    as: T[A],
    bsDep: DepTB[T, A, B],
    f: (A, B) => X
):
  def irregComprehensionDerived[Z](using c: ComprehensionB[S]): (X => Z) => T[Z] =
    c.irregularDerived(as, bsDep, f, _)

case class VariantRectDerivedMultipleB[T[_]: Mappable, U[_]: Mappable, X, A, B, R[_, _, _], S[_, _, _]](
    as: T[A],
    bs: U[B],
    f: (A, B) => X
):
  def rectComprehensionDerivedMultiple[Z](using c: ComprehensionB[R]) =
    c.rectDerivedMultiple(as, bs, f)

case class VariantSetRectangularB[X, A, B](s1: Set[A], s2: Set[B], f: (A, B) => X)
    extends VariantBT[X, A, B, SetB, SetB]:
  def as = Random.shuffle(s1.toList)
  def bs = Random.shuffle(s2.toList)
  def bsDep = (a: A) => bs
  def rectComprehension[Z](using c: ComprehensionB[SetB], t: ClassTag[Z]) = c.rectangular(as, bs, f, _)
  def irregComprehension[Z](using c: ComprehensionB[SetB]) = c.irregular(as, bsDep, f, _)

case class VariantSetIrregB[X, A, B](s1: Set[A], s2Dep: DepSetB[A, B], f: (A, B) => X)
    extends VariantBT[X, A, B, SetB, SetB]:
  def as = Random.shuffle(s1.toList)
  def bsDep = a => Random.shuffle(s2Dep(a).toList)

  def rectComprehension[Z](using c: ComprehensionB[SetB], t: ClassTag[Z]) = ???
  def irregComprehension[Z](using c: ComprehensionB[SetB]) = c.irregular(as, bsDep, f, _)

trait VariantCT[X, A, B, C, R[_, _, _, _], S[_, _, _, _]]:
  def as: Seq[A]
  def bsDep: DepSeqB[A, B]
  def csDep: DepSeqC[A, B, C]
  def f: (A, B, C) => X

  def rectComprehension[Z](using c: ComprehensionC[R], t: ClassTag[Z]): (X => Z) => R[Z, A, B, C]
  def irregComprehension[Z](using c: ComprehensionC[S]): (X => Z) => S[Z, A, B, C]

case class VariantRectC[X, A, B, C, R[_, _, _, _], S[_, _, _, _]](as: Seq[A], bs: Seq[B], cs: Seq[C], f: (A, B, C) => X)
    extends VariantCT[X, A, B, C, R, S]:
  def bsDep = (a: A) => bs
  def csDep = (a: A, b: B) => cs
  def rectComprehension[Z](using c: ComprehensionC[R], t: ClassTag[Z]) = c.rectangular(as, bs, cs, f, _)
  def irregComprehension[Z](using c: ComprehensionC[S]) = c.irregular(as, bsDep, csDep, f, _)

case class VariantIrregC[X, A, B, C, R[_, _, _, _], S[_, _, _, _]](
    as: Seq[A],
    bsDep: DepSeqB[A, B],
    csDep: DepSeqC[A, B, C],
    f: (A, B, C) => X
) extends VariantCT[X, A, B, C, R, S]:
  def rectComprehension[Z](using c: ComprehensionC[R], t: ClassTag[Z]) = ???
  def irregComprehension[Z](using c: ComprehensionC[S]): (X => Z) => S[Z, A, B, C] =
    c.irregular(as, bsDep, csDep, f, _)

class VariantRectDerivedC[T[_]: Mappable, X, A, B, C, R[_, _, _, _], S[_, _, _, _]](
    as: => T[A],
    bs: => T[B],
    cs: => T[C],
    f: (A, B, C) => X
):
  def rectComprehensionDerived[Z](using c: ComprehensionC[S]): (X => Z) => T[Z] =
    c.rectDerived(as, bs, cs, f, _)

case class VariantIrregDerivedC[T[_]: Mappable, X, A, B, C, R[_, _, _, _], S[_, _, _, _]](
    as: T[A],
    bsDep: DepTB[T, A, B],
    csDep: DepTC[T, A, B, C],
    f: (A, B, C) => X
):
  def irregComprehensionDerived[Z](using c: ComprehensionC[S]): (X => Z) => T[Z] =
    c.irregularDerived(as, bsDep, csDep, f, _)

case class VariantRectDerivedMultipleC[T[_]: Mappable, U[_]: Mappable, V[_]: Mappable, X, A, B, C, R[_, _, _, _], S[
    _,
    _,
    _,
    _
]](
    as: T[A],
    bs: U[B],
    cs: V[C],
    f: (A, B, C) => X
):
  def rectComprehensionDerivedMultiple[Z](using c: ComprehensionC[R]) =
    c.rectDerivedMultiple(as, bs, cs, f)

case class VariantSetRectangularC[X, A, B, C](s1: Set[A], s2: Set[B], s3: Set[C], f: (A, B, C) => X)
    extends VariantCT[X, A, B, C, SetC, SetC]:
  def as = Random.shuffle(s1.toList)
  def bs = Random.shuffle(s2.toList)
  def cs = Random.shuffle(s3.toList)
  def bsDep = (a: A) => bs
  def csDep = (a: A, b: B) => cs
  def rectComprehension[Z](using c: ComprehensionC[SetC], t: ClassTag[Z]) = c.rectangular(as, bs, cs, f, _)
  def irregComprehension[Z](using c: ComprehensionC[SetC]) = c.irregular(as, bsDep, csDep, f, _)

case class VariantSetIrregC[X, A, B, C](s1: Set[A], s2Dep: DepSetB[A, B], s3Dep: DepSetC[A, B, C], f: (A, B, C) => X)
    extends VariantCT[X, A, B, C, SetC, SetC]:
  def as = Random.shuffle(s1.toList)
  def bsDep = a => Random.shuffle(s2Dep(a).toList)
  def csDep = (a, b) => Random.shuffle(s3Dep(a, b).toList)

  def rectComprehension[Z](using c: ComprehensionC[SetC], t: ClassTag[Z]) = ???
  def irregComprehension[Z](using c: ComprehensionC[SetC]) = c.irregular(as, bsDep, csDep, f, _)

trait VariantDT[X, A, B, C, D, R[_, _, _, _, _], S[_, _, _, _, _]]:
  def as: Seq[A]
  def bsDep: DepSeqB[A, B]
  def csDep: DepSeqC[A, B, C]
  def dsDep: DepSeqD[A, B, C, D]
  def f: (A, B, C, D) => X

  def rectComprehension[Z](using c: ComprehensionD[R], t: ClassTag[Z]): (X => Z) => R[Z, A, B, C, D]
  def irregComprehension[Z](using c: ComprehensionD[S]): (X => Z) => S[Z, A, B, C, D]

case class VariantRectD[X, A, B, C, D, R[_, _, _, _, _], S[_, _, _, _, _]](
    as: Seq[A],
    bs: Seq[B],
    cs: Seq[C],
    ds: Seq[D],
    f: (A, B, C, D) => X
) extends VariantDT[X, A, B, C, D, R, S]:
  def bsDep = (a: A) => bs
  def csDep = (a: A, b: B) => cs
  def dsDep = (a: A, b: B, c: C) => ds
  def rectComprehension[Z](using c: ComprehensionD[R], t: ClassTag[Z]) = c.rectangular(as, bs, cs, ds, f, _)
  def irregComprehension[Z](using c: ComprehensionD[S]) = c.irregular(as, bsDep, csDep, dsDep, f, _)

case class VariantIrregD[X, A, B, C, D, R[_, _, _, _, _], S[_, _, _, _, _]](
    as: Seq[A],
    bsDep: DepSeqB[A, B],
    csDep: DepSeqC[A, B, C],
    dsDep: DepSeqD[A, B, C, D],
    f: (A, B, C, D) => X
) extends VariantDT[X, A, B, C, D, R, S]:
  def rectComprehension[Z](using c: ComprehensionD[R], t: ClassTag[Z]) = ???
  def irregComprehension[Z](using c: ComprehensionD[S]) = c.irregular(as, bsDep, csDep, dsDep, f, _)

case class VariantRectDerivedD[T[_]: Mappable, X, A, B, C, D, R[_, _, _, _, _], S[_, _, _, _, _]](
    as: T[A],
    bs: T[B],
    cs: T[C],
    ds: T[D],
    f: (A, B, C, D) => X
):
  def rectComprehensionDerived[Z](using c: ComprehensionD[S]): (X => Z) => T[Z] =
    c.rectDerived(as, bs, cs, ds, f, _)

case class VariantIrregDerivedD[T[_]: Mappable, X, A, B, C, D, R[_, _, _, _, _], S[_, _, _, _, _]](
    as: T[A],
    bsDep: DepTB[T, A, B],
    csDep: DepTC[T, A, B, C],
    dsDep: DepTD[T, A, B, C, D],
    f: (A, B, C, D) => X
):
  def irregComprehensionDerived[Z](using c: ComprehensionD[S]): (X => Z) => T[Z] =
    c.irregularDerived(as, bsDep, csDep, dsDep, f, _)

case class VariantSetIrregD[X, A, B, C, D](
    s1: Set[A],
    s2Dep: DepSetB[A, B],
    s3Dep: DepSetC[A, B, C],
    s4Dep: DepSetD[A, B, C, D],
    f: (A, B, C, D) => X
) extends VariantDT[X, A, B, C, D, SetD, SetD]:
  def as = Random.shuffle(s1.toList)
  def bsDep = a => Random.shuffle(s2Dep(a).toList)
  def csDep = (a, b) => Random.shuffle(s3Dep(a, b).toList)
  def dsDep = (a, b, c) => Random.shuffle(s4Dep(a, b, c).toList)
  def rectComprehension[Z](using c: ComprehensionD[SetD], t: ClassTag[Z]) = ???
  def irregComprehension[Z](using c: ComprehensionD[SetD]) = c.irregular(as, bsDep, csDep, dsDep, f, _)

trait VariantET[X, A, B, C, D, E, R[_, _, _, _, _, _], S[_, _, _, _, _, _]]:
  def as: Seq[A]
  def bsDep: DepSeqB[A, B]
  def csDep: DepSeqC[A, B, C]
  def dsDep: DepSeqD[A, B, C, D]
  def esDep: DepSeqE[A, B, C, D, E]

  def f: (A, B, C, D, E) => X

  def rectComprehension[Z](using c: ComprehensionE[R], t: ClassTag[Z]): (X => Z) => R[Z, A, B, C, D, E]
  def irregComprehension[Z](using c: ComprehensionE[S]): (X => Z) => S[Z, A, B, C, D, E]

case class VariantRectE[X, A, B, C, D, E, R[_, _, _, _, _, _], S[_, _, _, _, _, _]](
    as: Seq[A],
    bs: Seq[B],
    cs: Seq[C],
    ds: Seq[D],
    es: Seq[E],
    f: (A, B, C, D, E) => X
) extends VariantET[X, A, B, C, D, E, R, S]:
  def bsDep = (a: A) => bs
  def csDep = (a: A, b: B) => cs
  def dsDep = (a: A, b: B, c: C) => ds
  def esDep = (a: A, b: B, c: C, d: D) => es
  def rectComprehension[Z](using c: ComprehensionE[R], t: ClassTag[Z]) = c.rectangular(as, bs, cs, ds, es, f, _)
  def irregComprehension[Z](using c: ComprehensionE[S]) = c.irregular(as, bsDep, csDep, dsDep, esDep, f, _)

case class VariantIrregE[X, A, B, C, D, E, R[_, _, _, _, _, _], S[_, _, _, _, _, _]](
    as: Seq[A],
    bsDep: DepSeqB[A, B],
    csDep: DepSeqC[A, B, C],
    dsDep: DepSeqD[A, B, C, D],
    esDep: DepSeqE[A, B, C, D, E],
    f: (A, B, C, D, E) => X
) extends VariantET[X, A, B, C, D, E, R, S]:
  def rectComprehension[Z](using c: ComprehensionE[R], t: ClassTag[Z]) = ???
  def irregComprehension[Z](using c: ComprehensionE[S]) = c.irregular(as, bsDep, csDep, dsDep, esDep, f, _)

case class VariantRectDerivedE[T[_]: Mappable, X, A, B, C, D, E, R[_, _, _, _, _, _], S[_, _, _, _, _, _]](
    as: T[A],
    bs: T[B],
    cs: T[C],
    ds: T[D],
    es: T[E],
    f: (A, B, C, D, E) => X
):
  def rectComprehensionDerived[Z](using c: ComprehensionE[S]): (X => Z) => T[Z] =
    c.rectDerived(as, bs, cs, ds, es, f, _)

case class VariantIrregDerivedE[T[_]: Mappable, X, A, B, C, D, E, R[_, _, _, _, _, _], S[_, _, _, _, _, _]](
    as: T[A],
    bsDep: DepTB[T, A, B],
    csDep: DepTC[T, A, B, C],
    dsDep: DepTD[T, A, B, C, D],
    esDep: DepTE[T, A, B, C, D, E],
    f: (A, B, C, D, E) => X
):
  def irregComprehensionDerived[Z](using c: ComprehensionE[S]): (X => Z) => T[Z] =
    c.irregularDerived(as, bsDep, csDep, dsDep, esDep, f, _)

trait VariantFT[X, A, B, C, D, E, F, R[_, _, _, _, _, _, _], S[_, _, _, _, _, _, _]]:
  def as: Seq[A]
  def bsDep: DepSeqB[A, B]
  def csDep: DepSeqC[A, B, C]
  def dsDep: DepSeqD[A, B, C, D]
  def esDep: DepSeqE[A, B, C, D, E]
  def fsDep: DepSeqF[A, B, C, D, E, F]

  def f: (A, B, C, D, E, F) => X

  def rectComprehension[Z](using c: ComprehensionF[R], t: ClassTag[Z]): (X => Z) => R[Z, A, B, C, D, E, F]
  def irregComprehension[Z](using c: ComprehensionF[S]): (X => Z) => S[Z, A, B, C, D, E, F]

case class VariantRectF[X, A, B, C, D, E, F, R[_, _, _, _, _, _, _], S[_, _, _, _, _, _, _]](
    as: Seq[A],
    bs: Seq[B],
    cs: Seq[C],
    ds: Seq[D],
    es: Seq[E],
    fs: Seq[F],
    f: (A, B, C, D, E, F) => X
) extends VariantFT[X, A, B, C, D, E, F, R, S]:
  def bsDep = (a: A) => bs
  def csDep = (a: A, b: B) => cs
  def dsDep = (a: A, b: B, c: C) => ds
  def esDep = (a: A, b: B, c: C, d: D) => es
  def fsDep = (a: A, b: B, c: C, d: D, e: E) => fs
  def rectComprehension[Z](using c: ComprehensionF[R], t: ClassTag[Z]) = c.rectangular(as, bs, cs, ds, es, fs, f, _)
  def irregComprehension[Z](using c: ComprehensionF[S]) = c.irregular(as, bsDep, csDep, dsDep, esDep, fsDep, f, _)

case class VariantIrregF[X, A, B, C, D, E, F, R[_, _, _, _, _, _, _], S[_, _, _, _, _, _, _]](
    as: Seq[A],
    bsDep: DepSeqB[A, B],
    csDep: DepSeqC[A, B, C],
    dsDep: DepSeqD[A, B, C, D],
    esDep: DepSeqE[A, B, C, D, E],
    fsDep: DepSeqF[A, B, C, D, E, F],
    f: (A, B, C, D, E, F) => X
) extends VariantFT[X, A, B, C, D, E, F, R, S]:
  def rectComprehension[Z](using c: ComprehensionF[R], t: ClassTag[Z]) = ???
  def irregComprehension[Z](using c: ComprehensionF[S]) = c.irregular(as, bsDep, csDep, dsDep, esDep, fsDep, f, _)

case class VariantRectDerivedF[T[_]: Mappable, X, A, B, C, D, E, F, R[_, _, _, _, _, _, _], S[_, _, _, _, _, _, _]](
    as: T[A],
    bs: T[B],
    cs: T[C],
    ds: T[D],
    es: T[E],
    fs: T[F],
    f: (A, B, C, D, E, F) => X
):
  def rectComprehensionDerived[Z](using c: ComprehensionF[S]): (X => Z) => T[Z] =
    c.rectDerived(as, bs, cs, ds, es, fs, f, _)

case class VariantIrregDerivedF[T[_]: Mappable, X, A, B, C, D, E, F, R[_, _, _, _, _, _, _], S[_, _, _, _, _, _, _]](
    as: T[A],
    bsDep: DepTB[T, A, B],
    csDep: DepTC[T, A, B, C],
    dsDep: DepTD[T, A, B, C, D],
    esDep: DepTE[T, A, B, C, D, E],
    fsDep: DepTF[T, A, B, C, D, E, F],
    f: (A, B, C, D, E, F) => X
):
  def irregComprehensionDerived[Z](using c: ComprehensionF[S]): (X => Z) => T[Z] =
    c.irregularDerived(as, bsDep, csDep, dsDep, esDep, fsDep, f, _)

trait VariantGT[X, A, B, C, D, E, F, G, R[_, _, _, _, _, _, _, _], S[_, _, _, _, _, _, _, _]]:
  def as: Seq[A]
  def bsDep: DepSeqB[A, B]
  def csDep: DepSeqC[A, B, C]
  def dsDep: DepSeqD[A, B, C, D]
  def esDep: DepSeqE[A, B, C, D, E]
  def fsDep: DepSeqF[A, B, C, D, E, F]
  def gsDep: DepSeqG[A, B, C, D, E, F, G]

  def f: (A, B, C, D, E, F, G) => X

  def rectComprehension[Z](using c: ComprehensionG[R], t: ClassTag[Z]): (X => Z) => R[Z, A, B, C, D, E, F, G]
  def irregComprehension[Z](using c: ComprehensionG[S]): (X => Z) => S[Z, A, B, C, D, E, F, G]

case class VariantRectG[X, A, B, C, D, E, F, G, R[_, _, _, _, _, _, _, _], S[_, _, _, _, _, _, _, _]](
    as: Seq[A],
    bs: Seq[B],
    cs: Seq[C],
    ds: Seq[D],
    es: Seq[E],
    fs: Seq[F],
    gs: Seq[G],
    f: (A, B, C, D, E, F, G) => X
) extends VariantGT[X, A, B, C, D, E, F, G, R, S]:
  def bsDep = (a: A) => bs
  def csDep = (a: A, b: B) => cs
  def dsDep = (a: A, b: B, c: C) => ds
  def esDep = (a: A, b: B, c: C, d: D) => es
  def fsDep = (a: A, b: B, c: C, d: D, e: E) => fs
  def gsDep = (a: A, b: B, c: C, d: D, e: E, f0: F) => gs
  def rectComprehension[Z](using c: ComprehensionG[R], t: ClassTag[Z]) = c.rectangular(as, bs, cs, ds, es, fs, gs, f, _)
  def irregComprehension[Z](using c: ComprehensionG[S]) =
    c.irregular(as, bsDep, csDep, dsDep, esDep, fsDep, gsDep, f, _)

case class VariantIrregG[X, A, B, C, D, E, F, G, R[_, _, _, _, _, _, _, _], S[_, _, _, _, _, _, _, _]](
    as: Seq[A],
    bsDep: DepSeqB[A, B],
    csDep: DepSeqC[A, B, C],
    dsDep: DepSeqD[A, B, C, D],
    esDep: DepSeqE[A, B, C, D, E],
    fsDep: DepSeqF[A, B, C, D, E, F],
    gsDep: DepSeqG[A, B, C, D, E, F, G],
    f: (A, B, C, D, E, F, G) => X
) extends VariantGT[X, A, B, C, D, E, F, G, R, S]:
  def rectComprehension[Z](using c: ComprehensionG[R], t: ClassTag[Z]) = ???
  def irregComprehension[Z](using c: ComprehensionG[S]) =
    c.irregular(as, bsDep, csDep, dsDep, esDep, fsDep, gsDep, f, _)

case class VariantRectDerivedG[T[_], X, A, B, C, D, E, F, G, R[_, _, _, _, _, _, _, _], S[_, _, _, _, _, _, _, _]](
    as: T[A],
    bs: T[B],
    cs: T[C],
    ds: T[D],
    es: T[E],
    fs: T[F],
    gs: T[G],
    f: (A, B, C, D, E, F, G) => X
)(using Mappable[T]):
  def rectComprehensionDerived[Z](using c: ComprehensionG[S]): (X => Z) => T[Z] =
    c.rectDerived(as, bs, cs, ds, es, fs, gs, f, _)

case class VariantIrregDerivedG[T[_], X, A, B, C, D, E, F, G, R[_, _, _, _, _, _, _, _], S[_, _, _, _, _, _, _, _]](
    as: T[A],
    bsDep: DepTB[T, A, B],
    csDep: DepTC[T, A, B, C],
    dsDep: DepTD[T, A, B, C, D],
    esDep: DepTE[T, A, B, C, D, E],
    fsDep: DepTF[T, A, B, C, D, E, F],
    gsDep: DepTG[T, A, B, C, D, E, F, G],
    f: (A, B, C, D, E, F, G) => X
)(using Mappable[T]):
  def irregComprehensionDerived[Z](using c: ComprehensionG[S]): (X => Z) => T[Z] =
    c.irregularDerived(as, bsDep, csDep, dsDep, esDep, fsDep, gsDep, f, _)
