package comprehension.parallel

import augmented._
import comprehension._
import comprehension.given
import multiarray._
import shape._

import java.util.concurrent.ForkJoinPool
import scala.collection.parallel.{CollectionConverters, ForkJoinTaskSupport}
import scala.collection.parallel.CollectionConverters._
import scala.reflect.ClassTag

private val pool = new ForkJoinTaskSupport(new ForkJoinPool())

extension [A](xs: Seq[A])
  private def parWithPool =
    val p = xs.par
    p.tasksupport = pool
    p

given ComprehensionB[MultiArrayB] with
  def irregular[X, Z, A, B] = ???
  override def rectangular[X, Z, A, B](using ClassTag[Z]): RectComprehensionB[MultiArrayB, X, Z, A, B] =
    (as: Seq[A], bs: Seq[B], f: (A, B) => X, g: X => Z) =>
      val tuples = for a <- as; b <- bs yield (a, b)
      val arr = tuples.parWithPool.map(t => g(f.tupled(t))).seq.toArray
      multiArray(as, bs, arr)

given ComprehensionC[MultiArrayC] with
  def irregular[X, Z, A, B, C] = ???
  override def rectangular[X, Z, A, B, C](using ClassTag[Z]): RectComprehensionC[MultiArrayC, X, Z, A, B, C] =
    (as: Seq[A], bs: Seq[B], cs: Seq[C], f: (A, B, C) => X, g: X => Z) =>
      val tuples = for a <- as; b <- bs; c <- cs yield (a, b, c)
      val arr = tuples.parWithPool.map(t => g(f.tupled(t))).seq.toArray
      multiArray(as, bs, cs, arr)

given ComprehensionD[MultiArrayD] with
  def irregular[X, Z, A, B, C, D] = ???
  override def rectangular[X, Z, A, B, C, D](using ClassTag[Z]): RectComprehensionD[MultiArrayD, X, Z, A, B, C, D] =
    (as: Seq[A], bs: Seq[B], cs: Seq[C], ds: Seq[D], f: (A, B, C, D) => X, g: X => Z) =>
      val tuples = for a <- as; b <- bs; c <- cs; d <- ds yield (a, b, c, d)
      val arr = tuples.parWithPool.map(t => g(f.tupled(t))).seq.toArray
      multiArray(as, bs, cs, ds, arr)

given ComprehensionE[MultiArrayE] with
  def irregular[X, Z, A, B, C, D, E] = ???
  override def rectangular[X, Z, A, B, C, D, E](using
      ClassTag[Z]
  ): RectComprehensionE[MultiArrayE, X, Z, A, B, C, D, E] =
    (as: Seq[A], bs: Seq[B], cs: Seq[C], ds: Seq[D], es: Seq[E], f: (A, B, C, D, E) => X, g: X => Z) =>
      val tuples = for a <- as; b <- bs; c <- cs; d <- ds; e <- es yield (a, b, c, d, e)
      val arr = tuples.parWithPool.map(t => g(f.tupled(t))).seq.toArray
      multiArray(as, bs, cs, ds, es, arr)

given ComprehensionF[MultiArrayF] with
  def irregular[X, Z, A, B, C, D, E, F] = ???
  override def rectangular[X, Z, A, B, C, D, E, F](using
      ClassTag[Z]
  ): RectComprehensionF[MultiArrayF, X, Z, A, B, C, D, E, F] =
    (as: Seq[A], bs: Seq[B], cs: Seq[C], ds: Seq[D], es: Seq[E], fs: Seq[F], f: (A, B, C, D, E, F) => X, g: X => Z) =>
      val tuples = for a <- as; b <- bs; c <- cs; d <- ds; e <- es; f0 <- fs yield (a, b, c, d, e, f0)
      val arr = tuples.parWithPool.map(t => g(f.tupled(t))).seq.toArray
      MultiArrayF(as, bs, cs, ds, es, fs, arr)

given ComprehensionG[MultiArrayG] with
  def irregular[X, Z, A, B, C, D, E, F, G] = ???
  override def rectangular[X, Z, A, B, C, D, E, F, G](using
      ClassTag[Z]
  ): RectComprehensionG[MultiArrayG, X, Z, A, B, C, D, E, F, G] =
    (
        as: Seq[A],
        bs: Seq[B],
        cs: Seq[C],
        ds: Seq[D],
        es: Seq[E],
        fs: Seq[F],
        gs: Seq[G],
        f: (A, B, C, D, E, F, G) => X,
        g: X => Z
    ) =>
      val tuples =
        for a <- as; b <- bs; c <- cs; d <- ds; e <- es; f0 <- fs; g0 <- gs yield (a, b, c, d, e, f0, g0)
      val arr = tuples.parWithPool.map(t => g(f.tupled(t))).seq.toArray
      MultiArrayG(as, bs, cs, ds, es, fs, gs, arr)

given ComprehensionH[MultiArrayH] with
  def irregular[X, Z, A, B, C, D, E, F, G, H] = ???
  override def rectangular[X, Z, A, B, C, D, E, F, G, H](using
      ClassTag[Z]
  ): RectComprehensionH[MultiArrayH, X, Z, A, B, C, D, E, F, G, H] =
    (
        as: Seq[A],
        bs: Seq[B],
        cs: Seq[C],
        ds: Seq[D],
        es: Seq[E],
        fs: Seq[F],
        gs: Seq[G],
        hs: Seq[H],
        f: (A, B, C, D, E, F, G, H) => X,
        g: X => Z
    ) =>
      val tuples =
        for
          a <- as; b <- bs; c <- cs; d <- ds; e <- es; f0 <- fs; g0 <- gs; h <- hs
        yield (a, b, c, d, e, f0, g0, h)
      val arr = tuples.parWithPool.map(t => g(f.tupled(t))).seq.toArray
      MultiArrayH(as, bs, cs, ds, es, fs, gs, hs, arr)

given ComprehensionI[MultiArrayI] with
  def irregular[X, Z, A, B, C, D, E, F, G, H, I] = ???
  override def rectangular[X, Z, A, B, C, D, E, F, G, H, I](using
      ClassTag[Z]
  ): RectComprehensionI[MultiArrayI, X, Z, A, B, C, D, E, F, G, H, I] =
    (
        as: Seq[A],
        bs: Seq[B],
        cs: Seq[C],
        ds: Seq[D],
        es: Seq[E],
        fs: Seq[F],
        gs: Seq[G],
        hs: Seq[H],
        is: Seq[I],
        f: (A, B, C, D, E, F, G, H, I) => X,
        g: X => Z
    ) =>
      val tuples =
        for
          a <- as; b <- bs; c <- cs; d <- ds; e <- es; f0 <- fs; g0 <- gs; h <- hs; i <- is
        yield (a, b, c, d, e, f0, g0, h, i)
      val arr = tuples.parWithPool.map(t => g(f.tupled(t))).seq.toArray
      multiArray(as, bs, cs, ds, es, fs, gs, hs, is, arr)

given ComprehensionJ[MultiArrayJ] with
  def irregular[X, Z, A, B, C, D, E, F, G, H, I, J] = ???
  override def rectangular[X, Z, A, B, C, D, E, F, G, H, I, J](using
      ClassTag[Z]
  ): RectComprehensionJ[MultiArrayJ, X, Z, A, B, C, D, E, F, G, H, I, J] =
    (
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
        f: (A, B, C, D, E, F, G, H, I, J) => X,
        g: X => Z
    ) =>
      val tuples =
        for
          a <- as; b <- bs; c <- cs; d <- ds; e <- es; f0 <- fs; g0 <- gs; h <- hs; i <- is; j <- js
        yield (a, b, c, d, e, f0, g0, h, i, j)
      val arr = tuples.parWithPool.map(t => g(f.tupled(t))).seq.toArray
      multiArray(as, bs, cs, ds, es, fs, gs, hs, is, js, arr)

given ComprehensionK[MultiArrayK] with
  def irregular[X, Z, A, B, C, D, E, F, G, H, I, J, K] = ???
  override def rectangular[X, Z, A, B, C, D, E, F, G, H, I, J, K](using
      ClassTag[Z]
  ): RectComprehensionK[MultiArrayK, X, Z, A, B, C, D, E, F, G, H, I, J, K] =
    (
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
        f: (A, B, C, D, E, F, G, H, I, J, K) => X,
        g: X => Z
    ) =>
      val tuples =
        for
          a <- as; b <- bs; c <- cs; d <- ds; e <- es; f0 <- fs; g0 <- gs; h <- hs; i <- is; j <- js; k <- ks
        yield (a, b, c, d, e, f0, g0, h, i, j, k)
      val arr = tuples.parWithPool.map(t => g(f.tupled(t))).seq.toArray
      multiArray(as, bs, cs, ds, es, fs, gs, hs, is, js, ks, arr)

given AugmentB[MultiArrayB, SeqB] = AugmentB()
given AugmentC[MultiArrayC, SeqC] = AugmentC()
given AugmentD[MultiArrayD, SeqD] = AugmentD()
given AugmentE[MultiArrayE, SeqE] = AugmentE()
given AugmentF[MultiArrayF, SeqF] = AugmentF()
given AugmentG[MultiArrayG, SeqG] = AugmentG()
given AugmentH[MultiArrayH, SeqH] = AugmentH()
given AugmentI[MultiArrayI, SeqI] = AugmentI()
given AugmentJ[MultiArrayJ, SeqJ] = AugmentJ()
given AugmentK[MultiArrayK, SeqK] = AugmentK()
