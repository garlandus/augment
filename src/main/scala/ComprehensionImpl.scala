package comprehension

import multiarray._
import shape._

import scala.reflect.ClassTag

given ComprehensionA[SeqA] with
  def irregular[X, Z, A]: IrregComprehensionA[SeqA, X, Z, A] =
    (as: Seq[A], phi: A => Boolean, f: A => X, g: X => Z) =>
      as.withFilter(phi)
        .map:
          f andThen g

given ComprehensionA[ArrayNA] with
  def irregular[X, Z, A] = ???
  override def rectangular[X, Z, A](using ClassTag[Z]): RectComprehensionA[ArrayNA, X, Z, A] =
    (as: Seq[A], f: A => X, g: X => Z) =>
      as.map:
        f andThen g
      .toArray

given ComprehensionA[MapA] with
  def irregular[X, Z, A]: IrregComprehensionA[MapA, X, Z, A] =
    (as: Seq[A], phi: A => Boolean, f: A => X, g: X => Z) =>
      as.withFilter(phi)
        .map: a =>
          (a, g(f(a)))
        .toMap

given ComprehensionA[SetA] with
  def irregular[X, Z, A]: IrregComprehensionA[SetA, X, Z, A] =
    (as: Seq[A], phi: A => Boolean, f: A => X, g: X => Z) =>
      as.withFilter(phi)
        .map:
          f andThen g
        .toSet

given ComprehensionA[MultiArrayA] with
  def irregular[X, Z, A] = ???
  override def rectangular[X, Z, A](using ClassTag[Z]): RectComprehensionA[MultiArrayA, X, Z, A] =
    (as: Seq[A], f: A => X, g: X => Z) =>
      val m =
        as.map: a =>
          g(f(a))
      multiArray(as, m)

given ComprehensionB[SeqB] with
  def irregular[X, Z, A, B]: IrregComprehensionB[SeqB, X, Z, A, B] =
    (as: Seq[A], bsDep: DepSeqB[A, B], f: (A, B) => X, g: X => Z) =>
      as.flatMap: a =>
        bsDep(a).map: b =>
          g(f(a, b))

given ComprehensionB[SeqNB] with
  def irregular[X, Z, A, B]: IrregComprehensionB[SeqNB, X, Z, A, B] =
    (as: Seq[A], bsDep: DepSeqB[A, B], f: (A, B) => X, g: X => Z) =>
      as.map: a =>
        bsDep(a).map: b =>
          g(f(a, b))

given ComprehensionB[ArrayNB] with
  def irregular[X, Z, A, B] = ???
  override def rectangular[X, Z, A, B](using ClassTag[Z]): RectComprehensionB[ArrayNB, X, Z, A, B] =
    (as: Seq[A], bs: Seq[B], f: (A, B) => X, g: X => Z) =>
      as.map: a =>
        bs.map: b =>
          g(f(a, b))
        .toArray
      .toArray

given ComprehensionB[MapB] with
  def irregular[X, Z, A, B]: IrregComprehensionB[MapB, X, Z, A, B] =
    (as: Seq[A], bsDep: DepSeqB[A, B], f: (A, B) => X, g: X => Z) =>
      as.map: a =>
        (
          a,
          bsDep(a)
            .map: b =>
              (b, g(f(a, b)))
            .toMap
        )
      .toMap

given ComprehensionB[SetB] with
  def irregular[X, Z, A, B]: IrregComprehensionB[SetB, X, Z, A, B] =
    (as: Seq[A], bsDep: DepSeqB[A, B], f: (A, B) => X, g: X => Z) =>
      as.flatMap: a =>
        bsDep(a).map: b =>
          g(f(a, b))
      .toSet

given ComprehensionB[MultiArrayB] with
  def irregular[X, Z, A, B] = ???
  override def rectangular[X, Z, A, B](using ClassTag[Z]): RectComprehensionB[MultiArrayB, X, Z, A, B] =
    (as: Seq[A], bs: Seq[B], f: (A, B) => X, g: X => Z) =>
      val m =
        as.map: a =>
          bs.map: b =>
            g(f(a, b))
      multiArray(as, bs, m)

given ComprehensionC[SeqC] with
  def irregular[X, Z, A, B, C]: IrregComprehensionC[SeqC, X, Z, A, B, C] =
    (as: Seq[A], bsDep: DepSeqB[A, B], csDep: DepSeqC[A, B, C], f: (A, B, C) => X, g: X => Z) =>
      as.flatMap: a =>
        bsDep(a).flatMap: b =>
          csDep(a, b).map: c =>
            g(f(a, b, c))

given ComprehensionC[SeqNC] with
  def irregular[X, Z, A, B, C]: IrregComprehensionC[SeqNC, X, Z, A, B, C] =
    (as: Seq[A], bsDep: DepSeqB[A, B], csDep: DepSeqC[A, B, C], f: (A, B, C) => X, g: X => Z) =>
      as.map: a =>
        bsDep(a).map: b =>
          csDep(a, b).map: c =>
            g(f(a, b, c))

given ComprehensionC[ArrayNC] with
  def irregular[X, Z, A, B, C] = ???
  override def rectangular[X, Z, A, B, C](using ClassTag[Z]): RectComprehensionC[ArrayNC, X, Z, A, B, C] =
    (as: Seq[A], bs: Seq[B], cs: Seq[C], f: (A, B, C) => X, g: X => Z) =>
      as.map: a =>
        bs.map: b =>
          cs.map: c =>
            g(f(a, b, c))
          .toArray
        .toArray
      .toArray

given ComprehensionC[MapC] with
  def irregular[X, Z, A, B, C]: IrregComprehensionC[MapC, X, Z, A, B, C] =
    (as: Seq[A], bsDep: DepSeqB[A, B], csDep: DepSeqC[A, B, C], f: (A, B, C) => X, g: X => Z) =>
      as.map: a =>
        (a, bsDep(a).map(b => (b, csDep(a, b).map(c => (c, g(f(a, b, c)))).toMap)).toMap)
      .toMap

given ComprehensionC[MultiArrayC] with
  def irregular[X, Z, A, B, C] = ???
  override def rectangular[X, Z, A, B, C](using ClassTag[Z]): RectComprehensionC[MultiArrayC, X, Z, A, B, C] =
    (as: Seq[A], bs: Seq[B], cs: Seq[C], f: (A, B, C) => X, g: X => Z) =>
      val m = as.map: a =>
        bs.map: b =>
          cs.map: c =>
            g(f(a, b, c))
      multiArray(as, bs, cs, m)

given ComprehensionC[SetC] with
  def irregular[X, Z, A, B, C]: IrregComprehensionC[SetC, X, Z, A, B, C] =
    (as: Seq[A], bsDep: DepSeqB[A, B], csDep: DepSeqC[A, B, C], f: (A, B, C) => X, g: X => Z) =>
      as.flatMap: a =>
        bsDep(a).flatMap: b =>
          csDep(a, b).map: c =>
            g(f(a, b, c))
      .toSet

given ComprehensionD[SeqD] with
  def irregular[X, Z, A, B, C, D]: IrregComprehensionD[SeqD, X, Z, A, B, C, D] =
    (
        as: Seq[A],
        bsDep: DepSeqB[A, B],
        csDep: DepSeqC[A, B, C],
        dsDep: DepSeqD[A, B, C, D],
        f: (A, B, C, D) => X,
        g: X => Z
    ) =>
      as.flatMap: a =>
        bsDep(a).flatMap: b =>
          csDep(a, b).flatMap: c =>
            dsDep(a, b, c).map: d =>
              g(f(a, b, c, d))

given ComprehensionD[SeqND] with
  def irregular[X, Z, A, B, C, D]: IrregComprehensionD[SeqND, X, Z, A, B, C, D] =
    (
        as: Seq[A],
        bsDep: DepSeqB[A, B],
        csDep: DepSeqC[A, B, C],
        dsDep: DepSeqD[A, B, C, D],
        f: (A, B, C, D) => X,
        g: X => Z
    ) =>
      as.map: a =>
        bsDep(a).map: b =>
          csDep(a, b).map: c =>
            dsDep(a, b, c).map: d =>
              g(f(a, b, c, d))

given ComprehensionD[ArrayND] with
  def irregular[X, Z, A, B, C, D] = ???
  override def rectangular[X, Z, A, B, C, D](using ClassTag[Z]): RectComprehensionD[ArrayND, X, Z, A, B, C, D] =
    (as: Seq[A], bs: Seq[B], cs: Seq[C], ds: Seq[D], f: (A, B, C, D) => X, g: X => Z) =>
      as.map: a =>
        bs.map: b =>
          cs.map: c =>
            ds.map: d =>
              g(f(a, b, c, d))
            .toArray
          .toArray
        .toArray
      .toArray

given ComprehensionD[MapD] with
  def irregular[X, Z, A, B, C, D]: IrregComprehensionD[MapD, X, Z, A, B, C, D] =
    (
        as: Seq[A],
        bsDep: DepSeqB[A, B],
        csDep: DepSeqC[A, B, C],
        dsDep: DepSeqD[A, B, C, D],
        f: (A, B, C, D) => X,
        g: X => Z
    ) =>
      as.map: a =>
        (
          a,
          bsDep(a)
            .map(b => (b, csDep(a, b).map(c => (c, dsDep(a, b, c).map(d => (d, g(f(a, b, c, d)))).toMap)).toMap))
            .toMap
        )
      .toMap

given ComprehensionD[MultiArrayD] with
  def irregular[X, Z, A, B, C, D] = ???
  override def rectangular[X, Z, A, B, C, D](using ClassTag[Z]): RectComprehensionD[MultiArrayD, X, Z, A, B, C, D] =
    (as: Seq[A], bs: Seq[B], cs: Seq[C], ds: Seq[D], f: (A, B, C, D) => X, g: X => Z) =>
      val m =
        as.map: a =>
          bs.map: b =>
            cs.map: c =>
              ds.map: d =>
                g(f(a, b, c, d))
      multiArray(as, bs, cs, ds, m)

given ComprehensionD[SetD] with
  def irregular[X, Z, A, B, C, D]: IrregComprehensionD[SetD, X, Z, A, B, C, D] =
    (
        as: Seq[A],
        bsDep: DepSeqB[A, B],
        csDep: DepSeqC[A, B, C],
        dsDep: DepSeqD[A, B, C, D],
        f: (A, B, C, D) => X,
        g: X => Z
    ) =>
      as.flatMap: a =>
        bsDep(a).flatMap: b =>
          csDep(a, b).flatMap: c =>
            dsDep(a, b, c).map: d =>
              g(f(a, b, c, d))
      .toSet

given ComprehensionE[SeqE] with
  def irregular[X, Z, A, B, C, D, E]: IrregComprehensionE[SeqE, X, Z, A, B, C, D, E] =
    (
        as: Seq[A],
        bsDep: DepSeqB[A, B],
        csDep: DepSeqC[A, B, C],
        dsDep: DepSeqD[A, B, C, D],
        esDep: DepSeqE[A, B, C, D, E],
        f: (A, B, C, D, E) => X,
        g: X => Z
    ) =>
      as.flatMap: a =>
        bsDep(a).flatMap: b =>
          csDep(a, b).flatMap: c =>
            dsDep(a, b, c).flatMap: d =>
              esDep(a, b, c, d).map: e =>
                g(f(a, b, c, d, e))

given ComprehensionE[SeqNE] with
  def irregular[X, Z, A, B, C, D, E]: IrregComprehensionE[SeqNE, X, Z, A, B, C, D, E] =
    (
        as: Seq[A],
        bsDep: DepSeqB[A, B],
        csDep: DepSeqC[A, B, C],
        dsDep: DepSeqD[A, B, C, D],
        esDep: DepSeqE[A, B, C, D, E],
        f: (A, B, C, D, E) => X,
        g: X => Z
    ) =>
      as.map: a =>
        bsDep(a).map: b =>
          csDep(a, b).map: c =>
            dsDep(a, b, c).map: d =>
              esDep(a, b, c, d).map: e =>
                g(f(a, b, c, d, e))

given ComprehensionE[ArrayNE] with
  def irregular[X, Z, A, B, C, D, E] = ???
  override def rectangular[X, Z, A, B, C, D, E](using ClassTag[Z]): RectComprehensionE[ArrayNE, X, Z, A, B, C, D, E] =
    (as: Seq[A], bs: Seq[B], cs: Seq[C], ds: Seq[D], es: Seq[E], f: (A, B, C, D, E) => X, g: X => Z) =>
      as.map: a =>
        bs.map: b =>
          cs.map: c =>
            ds.map: d =>
              es.map: e =>
                g(f(a, b, c, d, e))
              .toArray
            .toArray
          .toArray
        .toArray
      .toArray

given ComprehensionE[MapE] with
  def irregular[X, Z, A, B, C, D, E]: IrregComprehensionE[MapE, X, Z, A, B, C, D, E] =
    (
        as: Seq[A],
        bsDep: DepSeqB[A, B],
        csDep: DepSeqC[A, B, C],
        dsDep: DepSeqD[A, B, C, D],
        esDep: DepSeqE[A, B, C, D, E],
        f: (A, B, C, D, E) => X,
        g: X => Z
    ) =>
      as.map: a =>
        (
          a,
          bsDep(a)
            .map(b =>
              (
                b,
                csDep(a, b)
                  .map(c =>
                    (c, dsDep(a, b, c).map(d => (d, esDep(a, b, c, d).map(e => (e, g(f(a, b, c, d, e)))).toMap)).toMap)
                  )
                  .toMap
              )
            )
            .toMap
        )
      .toMap

given ComprehensionE[MultiArrayE] with
  def irregular[X, Z, A, B, C, D, E] = ???
  override def rectangular[X, Z, A, B, C, D, E](using
      ClassTag[Z]
  ): RectComprehensionE[MultiArrayE, X, Z, A, B, C, D, E] =
    (as: Seq[A], bs: Seq[B], cs: Seq[C], ds: Seq[D], es: Seq[E], f: (A, B, C, D, E) => X, g: X => Z) =>
      val m =
        as.map: a =>
          bs.map: b =>
            cs.map: c =>
              ds.map: d =>
                es.map: e =>
                  g(f(a, b, c, d, e))
      multiArray(as, bs, cs, ds, es, m)

given ComprehensionE[SetE] with
  def irregular[X, Z, A, B, C, D, E]: IrregComprehensionE[SetE, X, Z, A, B, C, D, E] =
    (
        as: Seq[A],
        bsDep: DepSeqB[A, B],
        csDep: DepSeqC[A, B, C],
        dsDep: DepSeqD[A, B, C, D],
        esDep: DepSeqE[A, B, C, D, E],
        f: (A, B, C, D, E) => X,
        g: X => Z
    ) =>
      as.flatMap: a =>
        bsDep(a).flatMap: b =>
          csDep(a, b).flatMap: c =>
            dsDep(a, b, c).flatMap: d =>
              esDep(a, b, c, d).map: e =>
                g(f(a, b, c, d, e))
      .toSet

given ComprehensionF[SeqF] with
  def irregular[X, Z, A, B, C, D, E, F]: IrregComprehensionF[SeqF, X, Z, A, B, C, D, E, F] =
    (
        as: Seq[A],
        bsDep: DepSeqB[A, B],
        csDep: DepSeqC[A, B, C],
        dsDep: DepSeqD[A, B, C, D],
        esDep: DepSeqE[A, B, C, D, E],
        fsDep: DepSeqF[A, B, C, D, E, F],
        f: (A, B, C, D, E, F) => X,
        g: X => Z
    ) =>
      as.flatMap: a =>
        bsDep(a).flatMap: b =>
          csDep(a, b).flatMap: c =>
            dsDep(a, b, c).flatMap: d =>
              esDep(a, b, c, d).flatMap: e =>
                fsDep(a, b, c, d, e).map: f0 =>
                  g(f(a, b, c, d, e, f0))

given ComprehensionF[SeqNF] with
  def irregular[X, Z, A, B, C, D, E, F]: IrregComprehensionF[SeqNF, X, Z, A, B, C, D, E, F] =
    (
        as: Seq[A],
        bsDep: DepSeqB[A, B],
        csDep: DepSeqC[A, B, C],
        dsDep: DepSeqD[A, B, C, D],
        esDep: DepSeqE[A, B, C, D, E],
        fsDep: DepSeqF[A, B, C, D, E, F],
        f: (A, B, C, D, E, F) => X,
        g: X => Z
    ) =>
      as.map: a =>
        bsDep(a).map: b =>
          csDep(a, b).map: c =>
            dsDep(a, b, c).map: d =>
              esDep(a, b, c, d).map: e =>
                fsDep(a, b, c, d, e).map: f0 =>
                  g(f(a, b, c, d, e, f0))

given ComprehensionF[MultiArrayF] with
  def irregular[X, Z, A, B, C, D, E, F] = ???
  override def rectangular[X, Z, A, B, C, D, E, F](using
      ClassTag[Z]
  ): RectComprehensionF[MultiArrayF, X, Z, A, B, C, D, E, F] =
    (as: Seq[A], bs: Seq[B], cs: Seq[C], ds: Seq[D], es: Seq[E], fs: Seq[F], f: (A, B, C, D, E, F) => X, g: X => Z) =>
      val m =
        as.map: a =>
          bs.map: b =>
            cs.map: c =>
              ds.map: d =>
                es.map: e =>
                  fs.map: f0 =>
                    g(f(a, b, c, d, e, f0))
      val arr = m.flatten.flatten.flatten.flatten.flatten.toArray
      MultiArrayF(as, bs, cs, ds, es, fs, arr)

given ComprehensionG[SeqG] with
  def irregular[X, Z, A, B, C, D, E, F, G]: IrregComprehensionG[SeqG, X, Z, A, B, C, D, E, F, G] =
    (
        as: Seq[A],
        bsDep: DepSeqB[A, B],
        csDep: DepSeqC[A, B, C],
        dsDep: DepSeqD[A, B, C, D],
        esDep: DepSeqE[A, B, C, D, E],
        fsDep: DepSeqF[A, B, C, D, E, F],
        gsDep: DepSeqG[A, B, C, D, E, F, G],
        f: (A, B, C, D, E, F, G) => X,
        g: X => Z
    ) =>
      as.flatMap: a =>
        bsDep(a).flatMap: b =>
          csDep(a, b).flatMap: c =>
            dsDep(a, b, c).flatMap: d =>
              esDep(a, b, c, d).flatMap: e =>
                fsDep(a, b, c, d, e).flatMap: f0 =>
                  gsDep(a, b, c, d, e, f0).map: g0 =>
                    g(f(a, b, c, d, e, f0, g0))

given ComprehensionG[SeqNG] with
  def irregular[X, Z, A, B, C, D, E, F, G]: IrregComprehensionG[SeqNG, X, Z, A, B, C, D, E, F, G] =
    (
        as: Seq[A],
        bsDep: DepSeqB[A, B],
        csDep: DepSeqC[A, B, C],
        dsDep: DepSeqD[A, B, C, D],
        esDep: DepSeqE[A, B, C, D, E],
        fsDep: DepSeqF[A, B, C, D, E, F],
        gsDep: DepSeqG[A, B, C, D, E, F, G],
        f: (A, B, C, D, E, F, G) => X,
        g: X => Z
    ) =>
      as.map: a =>
        bsDep(a).map: b =>
          csDep(a, b).map: c =>
            dsDep(a, b, c).map: d =>
              esDep(a, b, c, d).map: e =>
                fsDep(a, b, c, d, e).map: f0 =>
                  gsDep(a, b, c, d, e, f0).map: g0 =>
                    g(f(a, b, c, d, e, f0, g0))

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
      val m =
        as.map: a =>
          bs.map: b =>
            cs.map: c =>
              ds.map: d =>
                es.map: e =>
                  fs.map: f0 =>
                    gs.map: g0 =>
                      g(f(a, b, c, d, e, f0, g0))
      MultiArrayG(as, bs, cs, ds, es, fs, gs, m.flatten.flatten.flatten.flatten.flatten.flatten.toArray)

given ComprehensionH[SeqH] with
  def irregular[X, Z, A, B, C, D, E, F, G, H]: IrregComprehensionH[SeqH, X, Z, A, B, C, D, E, F, G, H] =
    (
        as: Seq[A],
        bsDep: DepSeqB[A, B],
        csDep: DepSeqC[A, B, C],
        dsDep: DepSeqD[A, B, C, D],
        esDep: DepSeqE[A, B, C, D, E],
        fsDep: DepSeqF[A, B, C, D, E, F],
        gsDep: DepSeqG[A, B, C, D, E, F, G],
        hsDep: DepSeqH[A, B, C, D, E, F, G, H],
        f: (A, B, C, D, E, F, G, H) => X,
        g: X => Z
    ) =>
      as.flatMap: a =>
        bsDep(a).flatMap: b =>
          csDep(a, b).flatMap: c =>
            dsDep(a, b, c).flatMap: d =>
              esDep(a, b, c, d).flatMap: e =>
                fsDep(a, b, c, d, e).flatMap: f0 =>
                  gsDep(a, b, c, d, e, f0).flatMap: g0 =>
                    hsDep(a, b, c, d, e, f0, g0).map: h =>
                      g(f(a, b, c, d, e, f0, g0, h))

given ComprehensionH[SeqNH] with
  def irregular[X, Z, A, B, C, D, E, F, G, H]: IrregComprehensionH[SeqNH, X, Z, A, B, C, D, E, F, G, H] =
    (
        as: Seq[A],
        bsDep: DepSeqB[A, B],
        csDep: DepSeqC[A, B, C],
        dsDep: DepSeqD[A, B, C, D],
        esDep: DepSeqE[A, B, C, D, E],
        fsDep: DepSeqF[A, B, C, D, E, F],
        gsDep: DepSeqG[A, B, C, D, E, F, G],
        hsDep: DepSeqH[A, B, C, D, E, F, G, H],
        f: (A, B, C, D, E, F, G, H) => X,
        g: X => Z
    ) =>
      as.map: a =>
        bsDep(a).map: b =>
          csDep(a, b).map: c =>
            dsDep(a, b, c).map: d =>
              esDep(a, b, c, d).map: e =>
                fsDep(a, b, c, d, e).map: f0 =>
                  gsDep(a, b, c, d, e, f0).map: g0 =>
                    hsDep(a, b, c, d, e, f0, g0).map: h =>
                      g(f(a, b, c, d, e, f0, g0, h))

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
      val m =
        as.map: a =>
          bs.map: b =>
            cs.map: c =>
              ds.map: d =>
                es.map: e =>
                  fs.map: f0 =>
                    gs.map: g0 =>
                      hs.map: h =>
                        g(f(a, b, c, d, e, f0, g0, h))
      MultiArrayH(as, bs, cs, ds, es, fs, gs, hs, m.flatten.flatten.flatten.flatten.flatten.flatten.flatten.toArray)

given ComprehensionI[SeqI] with
  def irregular[X, Z, A, B, C, D, E, F, G, H, I]: IrregComprehensionI[SeqI, X, Z, A, B, C, D, E, F, G, H, I] =
    (
        as: Seq[A],
        bsDep: DepSeqB[A, B],
        csDep: DepSeqC[A, B, C],
        dsDep: DepSeqD[A, B, C, D],
        esDep: DepSeqE[A, B, C, D, E],
        fsDep: DepSeqF[A, B, C, D, E, F],
        gsDep: DepSeqG[A, B, C, D, E, F, G],
        hsDep: DepSeqH[A, B, C, D, E, F, G, H],
        isDep: DepSeqI[A, B, C, D, E, F, G, H, I],
        f: (A, B, C, D, E, F, G, H, I) => X,
        g: X => Z
    ) =>
      as.flatMap: a =>
        bsDep(a).flatMap: b =>
          csDep(a, b).flatMap: c =>
            dsDep(a, b, c).flatMap: d =>
              esDep(a, b, c, d).flatMap: e =>
                fsDep(a, b, c, d, e).flatMap: f0 =>
                  gsDep(a, b, c, d, e, f0).flatMap: g0 =>
                    hsDep(a, b, c, d, e, f0, g0).flatMap: h =>
                      isDep(a, b, c, d, e, f0, g0, h).map: i =>
                        g(f(a, b, c, d, e, f0, g0, h, i))

given ComprehensionI[SeqNI] with
  def irregular[X, Z, A, B, C, D, E, F, G, H, I]: IrregComprehensionI[SeqNI, X, Z, A, B, C, D, E, F, G, H, I] =
    (
        as: Seq[A],
        bsDep: DepSeqB[A, B],
        csDep: DepSeqC[A, B, C],
        dsDep: DepSeqD[A, B, C, D],
        esDep: DepSeqE[A, B, C, D, E],
        fsDep: DepSeqF[A, B, C, D, E, F],
        gsDep: DepSeqG[A, B, C, D, E, F, G],
        hsDep: DepSeqH[A, B, C, D, E, F, G, H],
        isDep: DepSeqI[A, B, C, D, E, F, G, H, I],
        f: (A, B, C, D, E, F, G, H, I) => X,
        g: X => Z
    ) =>
      as.map: a =>
        bsDep(a).map: b =>
          csDep(a, b).map: c =>
            dsDep(a, b, c).map: d =>
              esDep(a, b, c, d).map: e =>
                fsDep(a, b, c, d, e).map: f0 =>
                  gsDep(a, b, c, d, e, f0).map: g0 =>
                    hsDep(a, b, c, d, e, f0, g0).map: h =>
                      isDep(a, b, c, d, e, f0, g0, h).map: i =>
                        g(f(a, b, c, d, e, f0, g0, h, i))

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
      val m =
        as.map: a =>
          bs.map: b =>
            cs.map: c =>
              ds.map: d =>
                es.map: e =>
                  fs.map: f0 =>
                    gs.map: g0 =>
                      hs.map: h =>
                        is.map: i =>
                          g(f(a, b, c, d, e, f0, g0, h, i))
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
        m.flatten.flatten.flatten.flatten.flatten.flatten.flatten.flatten.toArray
      )

given ComprehensionJ[SeqJ] with
  def irregular[X, Z, A, B, C, D, E, F, G, H, I, J]: IrregComprehensionJ[SeqJ, X, Z, A, B, C, D, E, F, G, H, I, J] =
    (
        as: Seq[A],
        bsDep: DepSeqB[A, B],
        csDep: DepSeqC[A, B, C],
        dsDep: DepSeqD[A, B, C, D],
        esDep: DepSeqE[A, B, C, D, E],
        fsDep: DepSeqF[A, B, C, D, E, F],
        gsDep: DepSeqG[A, B, C, D, E, F, G],
        hsDep: DepSeqH[A, B, C, D, E, F, G, H],
        isDep: DepSeqI[A, B, C, D, E, F, G, H, I],
        jsDep: DepSeqJ[A, B, C, D, E, F, G, H, I, J],
        f: (A, B, C, D, E, F, G, H, I, J) => X,
        g: X => Z
    ) =>
      as.flatMap: a =>
        bsDep(a).flatMap: b =>
          csDep(a, b).flatMap: c =>
            dsDep(a, b, c).flatMap: d =>
              esDep(a, b, c, d).flatMap: e =>
                fsDep(a, b, c, d, e).flatMap: f0 =>
                  gsDep(a, b, c, d, e, f0).flatMap: g0 =>
                    hsDep(a, b, c, d, e, f0, g0).flatMap: h =>
                      isDep(a, b, c, d, e, f0, g0, h).flatMap: i =>
                        jsDep(a, b, c, d, e, f0, g0, h, i).map: j =>
                          g(f(a, b, c, d, e, f0, g0, h, i, j))

given ComprehensionJ[SeqNJ] with
  def irregular[X, Z, A, B, C, D, E, F, G, H, I, J]: IrregComprehensionJ[SeqNJ, X, Z, A, B, C, D, E, F, G, H, I, J] =
    (
        as: Seq[A],
        bsDep: DepSeqB[A, B],
        csDep: DepSeqC[A, B, C],
        dsDep: DepSeqD[A, B, C, D],
        esDep: DepSeqE[A, B, C, D, E],
        fsDep: DepSeqF[A, B, C, D, E, F],
        gsDep: DepSeqG[A, B, C, D, E, F, G],
        hsDep: DepSeqH[A, B, C, D, E, F, G, H],
        isDep: DepSeqI[A, B, C, D, E, F, G, H, I],
        jsDep: DepSeqJ[A, B, C, D, E, F, G, H, I, J],
        f: (A, B, C, D, E, F, G, H, I, J) => X,
        g: X => Z
    ) =>
      as.map: a =>
        bsDep(a).map: b =>
          csDep(a, b).map: c =>
            dsDep(a, b, c).map: d =>
              esDep(a, b, c, d).map: e =>
                fsDep(a, b, c, d, e).map: f0 =>
                  gsDep(a, b, c, d, e, f0).map: g0 =>
                    hsDep(a, b, c, d, e, f0, g0).map: h =>
                      isDep(a, b, c, d, e, f0, g0, h).map: i =>
                        jsDep(a, b, c, d, e, f0, g0, h, i).map: j =>
                          g(f(a, b, c, d, e, f0, g0, h, i, j))

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
      val m =
        as.map: a =>
          bs.map: b =>
            cs.map: c =>
              ds.map: d =>
                es.map: e =>
                  fs.map: f0 =>
                    gs.map: g0 =>
                      hs.map: h =>
                        is.map: i =>
                          js.map: j =>
                            g(f(a, b, c, d, e, f0, g0, h, i, j))
      val arr = m.flatten.flatten.flatten.flatten.flatten.flatten.flatten.flatten.flatten.toArray
      multiArray(as, bs, cs, ds, es, fs, gs, hs, is, js, arr)

given ComprehensionK[SeqK] with
  def irregular[X, Z, A, B, C, D, E, F, G, H, I, J, K]
      : IrregComprehensionK[SeqK, X, Z, A, B, C, D, E, F, G, H, I, J, K] =
    (
        as: Seq[A],
        bsDep: DepSeqB[A, B],
        csDep: DepSeqC[A, B, C],
        dsDep: DepSeqD[A, B, C, D],
        esDep: DepSeqE[A, B, C, D, E],
        fsDep: DepSeqF[A, B, C, D, E, F],
        gsDep: DepSeqG[A, B, C, D, E, F, G],
        hsDep: DepSeqH[A, B, C, D, E, F, G, H],
        isDep: DepSeqI[A, B, C, D, E, F, G, H, I],
        jsDep: DepSeqJ[A, B, C, D, E, F, G, H, I, J],
        ksDep: DepSeqK[A, B, C, D, E, F, G, H, I, J, K],
        f: (A, B, C, D, E, F, G, H, I, J, K) => X,
        g: X => Z
    ) =>
      as.flatMap: a =>
        bsDep(a).flatMap: b =>
          csDep(a, b).flatMap: c =>
            dsDep(a, b, c).flatMap: d =>
              esDep(a, b, c, d).flatMap: e =>
                fsDep(a, b, c, d, e).flatMap: f0 =>
                  gsDep(a, b, c, d, e, f0).flatMap: g0 =>
                    hsDep(a, b, c, d, e, f0, g0).flatMap: h =>
                      isDep(a, b, c, d, e, f0, g0, h).flatMap: i =>
                        jsDep(a, b, c, d, e, f0, g0, h, i).flatMap: j =>
                          ksDep(a, b, c, d, e, f0, g0, h, i, j).map: k =>
                            g(f(a, b, c, d, e, f0, g0, h, i, j, k))

given ComprehensionK[SeqNK] with
  def irregular[X, Z, A, B, C, D, E, F, G, H, I, J, K]
      : IrregComprehensionK[SeqNK, X, Z, A, B, C, D, E, F, G, H, I, J, K] =
    (
        as: Seq[A],
        bsDep: DepSeqB[A, B],
        csDep: DepSeqC[A, B, C],
        dsDep: DepSeqD[A, B, C, D],
        esDep: DepSeqE[A, B, C, D, E],
        fsDep: DepSeqF[A, B, C, D, E, F],
        gsDep: DepSeqG[A, B, C, D, E, F, G],
        hsDep: DepSeqH[A, B, C, D, E, F, G, H],
        isDep: DepSeqI[A, B, C, D, E, F, G, H, I],
        jsDep: DepSeqJ[A, B, C, D, E, F, G, H, I, J],
        ksDep: DepSeqK[A, B, C, D, E, F, G, H, I, J, K],
        f: (A, B, C, D, E, F, G, H, I, J, K) => X,
        g: X => Z
    ) =>
      as.map: a =>
        bsDep(a).map: b =>
          csDep(a, b).map: c =>
            dsDep(a, b, c).map: d =>
              esDep(a, b, c, d).map: e =>
                fsDep(a, b, c, d, e).map: f0 =>
                  gsDep(a, b, c, d, e, f0).map: g0 =>
                    hsDep(a, b, c, d, e, f0, g0).map: h =>
                      isDep(a, b, c, d, e, f0, g0, h).map: i =>
                        jsDep(a, b, c, d, e, f0, g0, h, i).map: j =>
                          ksDep(a, b, c, d, e, f0, g0, h, i, j).map: k =>
                            g(f(a, b, c, d, e, f0, g0, h, i, j, k))

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
      val m =
        as.map: a =>
          bs.map: b =>
            cs.map: c =>
              ds.map: d =>
                es.map: e =>
                  fs.map: f0 =>
                    gs.map: g0 =>
                      hs.map: h =>
                        is.map: i =>
                          js.map: j =>
                            ks.map: k =>
                              g(f(a, b, c, d, e, f0, g0, h, i, j, k))
      val arr = m.flatten.flatten.flatten.flatten.flatten.flatten.flatten.flatten.flatten.flatten.toArray
      multiArray(as, bs, cs, ds, es, fs, gs, hs, is, js, ks, arr)
