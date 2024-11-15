package comprehension

import mappable._
import shape._

import scala.reflect.ClassTag

trait ComprehensionA[F[_, _]]:

  def rectangular[X, Z, A](using ClassTag[Z]): RectComprehensionA[F, X, Z, A] =
    (as, f, g) => irregular(as, _ => true, f, g)
  def irregular[X, Z, A]: IrregComprehensionA[F, X, Z, A]

  def rectDerived[T[_], X, Z, A](using Mappable[T]): RectComprehensionDerivedA[F, T, X, Z, A] =
    def f(as: => T[A], f: => A => X, g: => X => Z)(using t: Mappable[T]) =
      t.map(as)((a: A) => g(f(a)))
    f

trait ComprehensionB[F[_, _, _]]:

  def rectangular[X, Z, A, B](using ClassTag[Z]): RectComprehensionB[F, X, Z, A, B] =
    (as, bs, f, g) => irregular(as, (a: A) => bs, f, g)
  def irregular[X, Z, A, B]: IrregComprehensionB[F, X, Z, A, B]

  def rectDerived[T[_], X, Z, A, B](using Mappable[T]): RectComprehensionDerivedB[F, T, X, Z, A, B] =
    def f(as: => T[A], bs: => T[B], f: => (A, B) => X, g: => X => Z)(using t: Mappable[T]) =
      t.product(as, bs).map((a, b) => g(f(a, b)))
    f

  def rectDerivedMultiple[T[_]: Mappable, U[_]: Mappable, X, Z, A, B](as: T[A], bs: U[B], f: (A, B) => X) =
    () =>
      as.map: a =>
        bs.map: b =>
          f(a, b)

  def irregularDerived[T[_], X, Z, A, B](using Mappable[T]): IrregComprehensionDerivedB[F, T, X, Z, A, B] =
    def f(as: T[A], bsDep: DepTB[T, A, B], f: (A, B) => X, g: X => Z)(using
        t: Mappable[T]
    ) =
      as.flatMap: a =>
        bsDep(a).map: b =>
          g(f(a, b))
    f

trait ComprehensionC[F[_, _, _, _]]:

  def rectangular[X, Z, A, B, C](using ClassTag[Z]): RectComprehensionC[F, X, Z, A, B, C] =
    (as, bs, cs, f, g) =>
      val bsDep = (a: A) => bs
      val csDep = (a: A, b: B) => cs
      irregular(as, bsDep, csDep, f, g)
  def irregular[X, Z, A, B, C]: IrregComprehensionC[F, X, Z, A, B, C]

  def rectDerived[T[_], X, Z, A, B, C](using Mappable[T]): RectComprehensionDerivedC[F, T, X, Z, A, B, C] =
    def f(as: => T[A], bs: => T[B], cs: => T[C], f: => (A, B, C) => X, g: => X => Z)(using t: Mappable[T]) =
      t.product(as, bs, cs).map((a, b, c) => g(f(a, b, c)))
    f

  def rectDerivedMultiple[T[_]: Mappable, U[_]: Mappable, V[_]: Mappable, X, Z, A, B, C](
      as: T[A],
      bs: U[B],
      cs: V[C],
      f: (A, B, C) => X
  ) =
    () =>
      as.map: a =>
        bs.map: b =>
          cs.map: c =>
            f(a, b, c)

  def irregularDerived[T[_], X, Z, A, B, C](using Mappable[T]): IrregComprehensionDerivedC[F, T, X, Z, A, B, C] =
    def f(as: T[A], bsDep: DepTB[T, A, B], csDep: DepTC[T, A, B, C], f: (A, B, C) => X, g: X => Z)(using
        t: Mappable[T]
    ) =
      as.flatMap: a =>
        bsDep(a).flatMap: b =>
          csDep(a, b).map: c =>
            g(f(a, b, c))
    f

trait ComprehensionD[F[_, _, _, _, _]]:

  def rectangular[X, Z, A, B, C, D](using ClassTag[Z]): RectComprehensionD[F, X, Z, A, B, C, D] =
    (as, bs, cs, ds, f, g) =>
      val bsDep = (a: A) => bs
      val csDep = (a: A, b: B) => cs
      val dsDep = (a: A, b: B, c: C) => ds
      irregular(as, bsDep, csDep, dsDep, f, g)

  def irregular[X, Z, A, B, C, D]: IrregComprehensionD[F, X, Z, A, B, C, D]

  def rectDerived[T[_], X, Z, A, B, C, D](using Mappable[T]): RectComprehensionDerivedD[F, T, X, Z, A, B, C, D] =
    def f(as: => T[A], bs: => T[B], cs: => T[C], ds: => T[D], f: => (A, B, C, D) => X, g: => X => Z)(using
        t: Mappable[T]
    ) =
      t.product(as, bs, cs, ds).map((a, b, c, d) => g(f(a, b, c, d)))
    f

  def irregularDerived[T[_], X, Z, A, B, C, D](using Mappable[T]): IrregComprehensionDerivedD[F, T, X, Z, A, B, C, D] =
    def f(
        as: T[A],
        bsDep: DepTB[T, A, B],
        csDep: DepTC[T, A, B, C],
        dsDep: DepTD[T, A, B, C, D],
        f: (A, B, C, D) => X,
        g: X => Z
    )(using t: Mappable[T]) =
      as.flatMap: a =>
        bsDep(a).flatMap: b =>
          csDep(a, b).flatMap: c =>
            dsDep(a, b, c).map: d =>
              g(f(a, b, c, d))
    f

trait ComprehensionE[F[_, _, _, _, _, _]]:

  def rectangular[X, Z, A, B, C, D, E](using ClassTag[Z]): RectComprehensionE[F, X, Z, A, B, C, D, E] =
    (as, bs, cs, ds, es, f, g) =>
      val bsDep = (a: A) => bs
      val csDep = (a: A, b: B) => cs
      val dsDep = (a: A, b: B, c: C) => ds
      val esDep = (a: A, b: B, c: C, d: D) => es
      irregular(as, bsDep, csDep, dsDep, esDep, f, g)

  def irregular[X, Z, A, B, C, D, E]: IrregComprehensionE[F, X, Z, A, B, C, D, E]

  def rectDerived[T[_], X, Z, A, B, C, D, E](using Mappable[T]): RectComprehensionDerivedE[F, T, X, Z, A, B, C, D, E] =
    def f(as: => T[A], bs: => T[B], cs: => T[C], ds: => T[D], es: => T[E], f: => (A, B, C, D, E) => X, g: => X => Z)(
        using t: Mappable[T]
    ) =
      t.product(as, bs, cs, ds, es).map((a, b, c, d, e) => g(f(a, b, c, d, e)))
    f

  def irregularDerived[T[_], X, Z, A, B, C, D, E](using
      Mappable[T]
  ): IrregComprehensionDerivedE[F, T, X, Z, A, B, C, D, E] =
    def f(
        as: T[A],
        bsDep: DepTB[T, A, B],
        csDep: DepTC[T, A, B, C],
        dsDep: DepTD[T, A, B, C, D],
        esDep: DepTE[T, A, B, C, D, E],
        f: (A, B, C, D, E) => X,
        g: X => Z
    )(using t: Mappable[T]) =
      as.flatMap: a =>
        bsDep(a).flatMap: b =>
          csDep(a, b).flatMap: c =>
            dsDep(a, b, c).flatMap: d =>
              esDep(a, b, c, d).map: e =>
                g(f(a, b, c, d, e))
    f

trait ComprehensionF[FF[_, _, _, _, _, _, _]]:

  def rectangular[X, Z, A, B, C, D, E, F](using ClassTag[Z]): RectComprehensionF[FF, X, Z, A, B, C, D, E, F] =
    (as, bs, cs, ds, es, fs, f, g) =>
      val bsDep = (a: A) => bs
      val csDep = (a: A, b: B) => cs
      val dsDep = (a: A, b: B, c: C) => ds
      val esDep = (a: A, b: B, c: C, d: D) => es
      val fsDep = (a: A, b: B, c: C, d: D, e: E) => fs
      irregular(as, bsDep, csDep, dsDep, esDep, fsDep, f, g)

  def irregular[X, Z, A, B, C, D, E, F]: IrregComprehensionF[FF, X, Z, A, B, C, D, E, F]

  def rectDerived[T[_], X, Z, A, B, C, D, E, F](using
      Mappable[T]
  ): RectComprehensionDerivedF[FF, T, X, Z, A, B, C, D, E, F] =
    def f(
        as: => T[A],
        bs: => T[B],
        cs: => T[C],
        ds: => T[D],
        es: => T[E],
        fs: => T[F],
        f: => (A, B, C, D, E, F) => X,
        g: => X => Z
    )(using
        t: Mappable[T]
    ) =
      t.product(as, bs, cs, ds, es, fs).map((a, b, c, d, e, f0) => g(f(a, b, c, d, e, f0)))
    f

  def irregularDerived[T[_], X, Z, A, B, C, D, E, F](using
      Mappable[T]
  ): IrregComprehensionDerivedF[FF, T, X, Z, A, B, C, D, E, F] =
    def f(
        as: T[A],
        bsDep: DepTB[T, A, B],
        csDep: DepTC[T, A, B, C],
        dsDep: DepTD[T, A, B, C, D],
        esDep: DepTE[T, A, B, C, D, E],
        fsDep: DepTF[T, A, B, C, D, E, F],
        f: (A, B, C, D, E, F) => X,
        g: X => Z
    )(using t: Mappable[T]) =
      as.flatMap: a =>
        bsDep(a).flatMap: b =>
          csDep(a, b).flatMap: c =>
            dsDep(a, b, c).flatMap: d =>
              esDep(a, b, c, d).flatMap: e =>
                fsDep(a, b, c, d, e).map: f0 =>
                  g(f(a, b, c, d, e, f0))
    f

trait ComprehensionG[FF[_, _, _, _, _, _, _, _]]:

  def rectangular[X, Z, A, B, C, D, E, F, G](using ClassTag[Z]): RectComprehensionG[FF, X, Z, A, B, C, D, E, F, G] =
    (as, bs, cs, ds, es, fs, gs, f, g) =>
      val bsDep = (a: A) => bs
      val csDep = (a: A, b: B) => cs
      val dsDep = (a: A, b: B, c: C) => ds
      val esDep = (a: A, b: B, c: C, d: D) => es
      val fsDep = (a: A, b: B, c: C, d: D, e: E) => fs
      val gsDep = (a: A, b: B, c: C, d: D, e: E, f0: F) => gs
      irregular(as, bsDep, csDep, dsDep, esDep, fsDep, gsDep, f, g)

  def irregular[X, Z, A, B, C, D, E, F, G]: IrregComprehensionG[FF, X, Z, A, B, C, D, E, F, G]

  def rectDerived[T[_], X, Z, A, B, C, D, E, F, G](using
      Mappable[T]
  ): RectComprehensionDerivedG[FF, T, X, Z, A, B, C, D, E, F, G] =
    def f(
        as: => T[A],
        bs: => T[B],
        cs: => T[C],
        ds: => T[D],
        es: => T[E],
        fs: => T[F],
        gs: => T[G],
        f: => (A, B, C, D, E, F, G) => X,
        g: => X => Z
    )(using
        t: Mappable[T]
    ) =
      t.product(as, bs, cs, ds, es, fs, gs).map((a, b, c, d, e, f0, g0) => g(f(a, b, c, d, e, f0, g0)))
    f

  def irregularDerived[T[_], X, Z, A, B, C, D, E, F, G](using
      Mappable[T]
  ): IrregComprehensionDerivedG[FF, T, X, Z, A, B, C, D, E, F, G] =
    def f(
        as: T[A],
        bsDep: DepTB[T, A, B],
        csDep: DepTC[T, A, B, C],
        dsDep: DepTD[T, A, B, C, D],
        esDep: DepTE[T, A, B, C, D, E],
        fsDep: DepTF[T, A, B, C, D, E, F],
        gsDep: DepTG[T, A, B, C, D, E, F, G],
        f: (A, B, C, D, E, F, G) => X,
        g: X => Z
    )(using t: Mappable[T]) =
      as.flatMap: a =>
        bsDep(a).flatMap: b =>
          csDep(a, b).flatMap: c =>
            dsDep(a, b, c).flatMap: d =>
              esDep(a, b, c, d).flatMap: e =>
                fsDep(a, b, c, d, e).flatMap: f0 =>
                  gsDep(a, b, c, d, e, f0).map: g0 =>
                    g(f(a, b, c, d, e, f0, g0))
    f
type RectComprehensionA[F[_, _], X, Z, A] = (Seq[A], A => X, X => Z) => F[Z, A]
type IrregComprehensionA[F[_, _], X, Z, A] = (Seq[A], A => Boolean, A => X, X => Z) => F[Z, A]

type RectComprehensionDerivedA[F[_, _], T[_], X, Z, A] = (=> T[A], A => X, X => Z) => T[Z]

type RectComprehensionB[F[_, _, _], X, Z, A, B] = (Seq[A], Seq[B], (A, B) => X, X => Z) => F[Z, A, B]
type IrregComprehensionB[F[_, _, _], X, Z, A, B] = (Seq[A], DepSeqB[A, B], (A, B) => X, X => Z) => F[Z, A, B]

type RectComprehensionDerivedB[F[_, _, _], T[_], X, Z, A, B] =
  (=> T[A], => T[B], (A, B) => X, X => Z) => T[Z]
type IrregComprehensionDerivedB[F[_, _, _], T[_], X, Z, A, B] =
  (T[A], DepTB[T, A, B], (A, B) => X, X => Z) => T[Z]

type RectComprehensionC[F[_, _, _, _], X, Z, A, B, C] =
  (Seq[A], Seq[B], Seq[C], (A, B, C) => X, X => Z) => F[Z, A, B, C]
type IrregComprehensionC[F[_, _, _, _], X, Z, A, B, C] =
  (Seq[A], DepSeqB[A, B], DepSeqC[A, B, C], (A, B, C) => X, X => Z) => F[Z, A, B, C]

type RectComprehensionDerivedC[F[_, _, _, _], T[_], X, Z, A, B, C] =
  (=> T[A], => T[B], => T[C], (A, B, C) => X, X => Z) => T[Z]

type IrregComprehensionDerivedC[F[_, _, _, _], T[_], X, Z, A, B, C] =
  (T[A], DepTB[T, A, B], DepTC[T, A, B, C], (A, B, C) => X, X => Z) => T[Z]

type RectComprehensionD[F[_, _, _, _, _], X, Z, A, B, C, D] =
  (Seq[A], Seq[B], Seq[C], Seq[D], (A, B, C, D) => X, X => Z) => F[Z, A, B, C, D]
type IrregComprehensionD[F[_, _, _, _, _], X, Z, A, B, C, D] =
  (Seq[A], DepSeqB[A, B], DepSeqC[A, B, C], DepSeqD[A, B, C, D], (A, B, C, D) => X, X => Z) => F[Z, A, B, C, D]

type RectComprehensionDerivedD[F[_, _, _, _, _], T[_], X, Z, A, B, C, D] =
  (=> T[A], => T[B], => T[C], => T[D], (A, B, C, D) => X, X => Z) => T[Z]
type IrregComprehensionDerivedD[F[_, _, _, _, _], T[_], X, Z, A, B, C, D] =
  (T[A], DepTB[T, A, B], DepTC[T, A, B, C], DepTD[T, A, B, C, D], (A, B, C, D) => X, X => Z) => T[Z]

type RectComprehensionE[F[_, _, _, _, _, _], X, Z, A, B, C, D, E] =
  (Seq[A], Seq[B], Seq[C], Seq[D], Seq[E], (A, B, C, D, E) => X, X => Z) => F[Z, A, B, C, D, E]
type IrregComprehensionE[F[_, _, _, _, _, _], X, Z, A, B, C, D, E] =
  (
      Seq[A],
      DepSeqB[A, B],
      DepSeqC[A, B, C],
      DepSeqD[A, B, C, D],
      DepSeqE[A, B, C, D, E],
      (A, B, C, D, E) => X,
      X => Z
  ) => F[Z, A, B, C, D, E]

type RectComprehensionDerivedE[F[_, _, _, _, _, _], T[_], X, Z, A, B, C, D, E] =
  (=> T[A], => T[B], => T[C], => T[D], => T[E], (A, B, C, D, E) => X, X => Z) => T[Z]
type IrregComprehensionDerivedE[F[_, _, _, _, _, _], T[_], X, Z, A, B, C, D, E] =
  (
      T[A],
      DepTB[T, A, B],
      DepTC[T, A, B, C],
      DepTD[T, A, B, C, D],
      DepTE[T, A, B, C, D, E],
      (A, B, C, D, E) => X,
      X => Z
  ) => T[Z]

type RectComprehensionF[FF[_, _, _, _, _, _, _], X, Z, A, B, C, D, E, F] =
  (Seq[A], Seq[B], Seq[C], Seq[D], Seq[E], Seq[F], (A, B, C, D, E, F) => X, X => Z) => FF[Z, A, B, C, D, E, F]
type IrregComprehensionF[FF[_, _, _, _, _, _, _], X, Z, A, B, C, D, E, F] =
  (
      Seq[A],
      DepSeqB[A, B],
      DepSeqC[A, B, C],
      DepSeqD[A, B, C, D],
      DepSeqE[A, B, C, D, E],
      DepSeqF[A, B, C, D, E, F],
      (A, B, C, D, E, F) => X,
      X => Z
  ) => FF[Z, A, B, C, D, E, F]

type RectComprehensionDerivedF[FF[_, _, _, _, _, _, _], T[_], X, Z, A, B, C, D, E, F] =
  (=> T[A], => T[B], => T[C], => T[D], => T[E], => T[F], (A, B, C, D, E, F) => X, X => Z) => T[Z]
type IrregComprehensionDerivedF[FF[_, _, _, _, _, _, _], T[_], X, Z, A, B, C, D, E, F] =
  (
      T[A],
      DepTB[T, A, B],
      DepTC[T, A, B, C],
      DepTD[T, A, B, C, D],
      DepTE[T, A, B, C, D, E],
      DepTF[T, A, B, C, D, E, F],
      (A, B, C, D, E, F) => X,
      X => Z
  ) => T[Z]

type RectComprehensionG[FF[_, _, _, _, _, _, _, _], X, Z, A, B, C, D, E, F, G] =
  (
      Seq[A],
      Seq[B],
      Seq[C],
      Seq[D],
      Seq[E],
      Seq[F],
      Seq[G],
      (A, B, C, D, E, F, G) => X,
      X => Z
  ) => FF[Z, A, B, C, D, E, F, G]
type IrregComprehensionG[FF[_, _, _, _, _, _, _, _], X, Z, A, B, C, D, E, F, G] =
  (
      Seq[A],
      DepSeqB[A, B],
      DepSeqC[A, B, C],
      DepSeqD[A, B, C, D],
      DepSeqE[A, B, C, D, E],
      DepSeqF[A, B, C, D, E, F],
      DepSeqG[A, B, C, D, E, F, G],
      (A, B, C, D, E, F, G) => X,
      X => Z
  ) => FF[Z, A, B, C, D, E, F, G]

type RectComprehensionDerivedG[FF[_, _, _, _, _, _, _, _], T[_], X, Z, A, B, C, D, E, F, G] =
  (=> T[A], => T[B], => T[C], => T[D], => T[E], => T[F], => T[G], (A, B, C, D, E, F, G) => X, X => Z) => T[Z]
type IrregComprehensionDerivedG[FF[_, _, _, _, _, _, _, _], T[_], X, Z, A, B, C, D, E, F, G] =
  (
      T[A],
      DepTB[T, A, B],
      DepTC[T, A, B, C],
      DepTD[T, A, B, C, D],
      DepTE[T, A, B, C, D, E],
      DepTF[T, A, B, C, D, E, F],
      DepTG[T, A, B, C, D, E, F, G],
      (A, B, C, D, E, F, G) => X,
      X => Z
  ) => T[Z]

case class ComprehensionsA[R[_, _], S[_, _]](rectangular: ComprehensionA[R], irregular: ComprehensionA[S])
case class ComprehensionsB[R[_, _, _], S[_, _, _]](rectangular: ComprehensionB[R], irregular: ComprehensionB[S])
case class ComprehensionsC[R[_, _, _, _], S[_, _, _, _]](rectangular: ComprehensionC[R], irregular: ComprehensionC[S])
case class ComprehensionsD[R[_, _, _, _, _], S[_, _, _, _, _]](
    rectangular: ComprehensionD[R],
    irregular: ComprehensionD[S]
)
case class ComprehensionsE[R[_, _, _, _, _, _], S[_, _, _, _, _, _]](
    rectangular: ComprehensionE[R],
    irregular: ComprehensionE[S]
)

object ComprehensionShapes:
  def apply[R[_, _], S[_, _]]()(using cx: ComprehensionA[R], cy: ComprehensionA[S]): ComprehensionsA[R, S] =
    ComprehensionsA(cx, cy)

  def apply[R[_, _, _], S[_, _, _]]()(using cx: ComprehensionB[R], cy: ComprehensionB[S]): ComprehensionsB[R, S] =
    ComprehensionsB(cx, cy)

  def apply[R[_, _, _, _], S[_, _, _, _]]()(using cx: ComprehensionC[R], cy: ComprehensionC[S]): ComprehensionsC[R, S] =
    ComprehensionsC(cx, cy)
  def apply[R[_, _, _, _, _], S[_, _, _, _, _]]()(using
      cx: ComprehensionD[R],
      cy: ComprehensionD[S]
  ): ComprehensionsD[R, S] =
    ComprehensionsD(cx, cy)
  def apply[R[_, _, _, _, _, _], S[_, _, _, _, _, _]]()(using
      cx: ComprehensionE[R],
      cy: ComprehensionE[S]
  ): ComprehensionsE[R, S] =
    ComprehensionsE(cx, cy)
