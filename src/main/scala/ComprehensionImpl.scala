package comprehension

import multiarray._
import shape._

import scala.reflect.ClassTag

given ComprehensionA[SeqA] with
  def irregular[X, Z, A]: IrregComprehensionA[SeqA, X, Z, A] =
    (as: Seq[A], f: A => X, g: X => Z) =>
      as.map:
        f andThen g

given ComprehensionA[ArrayNA] with
  def irregular[X, Z, A]: IrregComprehensionA[ArrayNA, X, Z, A] = ???
  override def rectangular[X, Z, A](using ClassTag[Z]): RectComprehensionA[ArrayNA, X, Z, A] =
    (as: Seq[A], f: A => X, g: X => Z) =>
      as.map:
        f andThen g
      .toArray

given ComprehensionA[MapA] with
  def irregular[X, Z, A]: IrregComprehensionA[MapA, X, Z, A] =
    (as: Seq[A], f: A => X, g: X => Z) =>
      as.map: a =>
        (a, g(f(a)))
      .toMap

given ComprehensionA[SetA] with
  def irregular[X, Z, A]: IrregComprehensionA[SetA, X, Z, A] =
    (as: Seq[A], f: A => X, g: X => Z) =>
      as.map:
        f andThen g
      .toSet

given ComprehensionA[MultiArrayA] with
  def irregular[X, Z, A]: IrregComprehensionA[MultiArrayA, X, Z, A] = ???
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
  def irregular[X, Z, A, B]: IrregComprehensionB[ArrayNB, X, Z, A, B] = ???
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
  def irregular[X, Z, A, B]: IrregComprehensionB[MultiArrayB, X, Z, A, B] = ???
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
  def irregular[X, Z, A, B, C]: IrregComprehensionC[ArrayNC, X, Z, A, B, C] = ???
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
  def irregular[X, Z, A, B, C]: IrregComprehensionC[MultiArrayC, X, Z, A, B, C] = ???
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
  def irregular[X, Z, A, B, C, D]: IrregComprehensionD[ArrayND, X, Z, A, B, C, D] = ???
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
  def irregular[X, Z, A, B, C, D]: IrregComprehensionD[MultiArrayD, X, Z, A, B, C, D] = ???
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
  def irregular[X, Z, A, B, C, D, E]: IrregComprehensionE[ArrayNE, X, Z, A, B, C, D, E] = ???
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
  def irregular[X, Z, A, B, C, D, E]: IrregComprehensionE[MultiArrayE, X, Z, A, B, C, D, E] = ???
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
