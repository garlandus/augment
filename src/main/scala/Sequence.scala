package augmented

import mappable._
import mappable.given
import shape._

import scala.reflect.ClassTag

import zio._
import mappablethirdparty.given

object sequence:

  /** 
    * Basic form: this always works, but can be unwieldy
    * 
    *     A
    *     A => T[B]
    *     (A, B) => T[C]
    *     (A, B, C) => T[D]
    *     (A, B, C, D) => T[E]
    *     ...
    * 
    * where T[X] can actually also be simply X, which then is "lifted" into T[X]
    */

  def apply[T[_]: Mappable, A](as: => Mixed[A, T])(using ta: ClassTag[A], pa: Plain[A]) =
    augment(id) applyMixedRect (as)

  def apply[T[_]: Mappable, A, B](as: => Mixed[A, T], bs: A => Mixed[B, T])(using
      ta: ClassTag[A],
      tb: ClassTag[B],
      pa: Plain[A],
      pb: Plain[B]
  ) =
    lastB applyMixed (as, bs)

  /** The one exception to the general diagonal form: T[B] is more common than A => T[B],
   * and having both seems to cause a "typing collision" (the other form is moved to sequenceAlt)
   */
  def apply[T[_]: Mappable, A, B, C](
      as: => Mixed[A, T],
      bs: => Mixed[B, T],
      cs: (A, B) => Mixed[C, T]
  )(using ta: ClassTag[A], tb: ClassTag[B], tc: ClassTag[C], pa: Plain[A], pb: Plain[B], pc: Plain[C]) =
    lastC applyMixed (as, _ => bs, cs)

  def apply[T[_]: Mappable, A, B, C, D](
      as: => Mixed[A, T],
      bs: A => Mixed[B, T],
      cs: (A, B) => Mixed[C, T],
      ds: (A, B, C) => Mixed[D, T]
  )(using
      ta: ClassTag[A],
      tb: ClassTag[B],
      tc: ClassTag[C],
      td: ClassTag[D],
      pa: Plain[A],
      pb: Plain[B],
      pc: Plain[C],
      pd: Plain[D]
  ) =
    val allPlainTypes = pa.isPlain && pb.isPlain && pc.isPlain && pd.isPlain
    lastD applyMixed (as, bs, cs, ds)

  def apply[T[_]: Mappable, A, B, C, D, E](
      as: => Mixed[A, T],
      bs: A => Mixed[B, T],
      cs: (A, B) => Mixed[C, T],
      ds: (A, B, C) => Mixed[D, T],
      es: (A, B, C, D) => Mixed[E, T]
  )(using
      ta: ClassTag[A],
      tb: ClassTag[B],
      tc: ClassTag[C],
      td: ClassTag[D],
      te: ClassTag[E],
      pa: Plain[A],
      pb: Plain[B],
      pc: Plain[C],
      pd: Plain[D],
      pe: Plain[E]
  ) =
    lastE applyMixed (as, bs, cs, ds, es)

  def apply[T[_]: Mappable, A, B, C, D, E, F](
      as: => Mixed[A, T],
      bs: A => Mixed[B, T],
      cs: (A, B) => Mixed[C, T],
      ds: (A, B, C) => Mixed[D, T],
      es: (A, B, C, D) => Mixed[E, T],
      fs: (A, B, C, D, E) => Mixed[F, T]
  )(using
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
      pf: Plain[F]
  ) =
    lastF applyMixed (as, bs, cs, ds, es, fs)

  def apply[T[_]: Mappable, A, B, C, D, E, F, G](
      as: => Mixed[A, T],
      bs: A => Mixed[B, T],
      cs: (A, B) => Mixed[C, T],
      ds: (A, B, C) => Mixed[D, T],
      es: (A, B, C, D) => Mixed[E, T],
      fs: (A, B, C, D, E) => Mixed[F, T],
      gs: (A, B, C, D, E, F) => Mixed[G, T]
  )(using
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
      pg: Plain[G]
  ) =
    lastG applyMixed (as, bs, cs, ds, es, fs, gs)

  /** 
    * "Threaded" form: each variable is passed through to the next function
    * 
    *     A
    *     A => T[B]
    *     B => T[C]
    *     C => T[D]
    *     D => T[E]
    *     ...
    * 
    * where T[X] is actually T[X] | X, with X to be "lifted" into T[X]
    */

  def apply[T[_]: Mappable, A, B, C](
      as: => Mixed[A, T],
      bs: A => Mixed[B, T],
      cs: B => Mixed[C, T]
  )(using ta: ClassTag[A], tb: ClassTag[B], tc: ClassTag[C], pa: Plain[A], pb: Plain[B], pc: Plain[C]) =
    lastC applyMixed (as, bs, (_, b) => cs(b))

  def apply[T[_]: Mappable, A, B, C, D](
      as: => Mixed[A, T],
      bs: A => Mixed[B, T],
      cs: B => Mixed[C, T],
      ds: C => Mixed[D, T]
  )(using
      ta: ClassTag[A],
      tb: ClassTag[B],
      tc: ClassTag[C],
      td: ClassTag[D],
      pa: Plain[A],
      pb: Plain[B],
      pc: Plain[C],
      pd: Plain[D]
  ) =
    lastD applyMixed (as, bs, (_, b) => cs(b), (_, _, c) => ds(c))

  def apply[T[_]: Mappable, A, B, C, D, E](
      as: => Mixed[A, T],
      bs: A => Mixed[B, T],
      cs: B => Mixed[C, T],
      ds: C => Mixed[D, T],
      es: D => Mixed[E, T]
  )(using
      ta: ClassTag[A],
      tb: ClassTag[B],
      tc: ClassTag[C],
      td: ClassTag[D],
      te: ClassTag[E],
      pa: Plain[A],
      pb: Plain[B],
      pc: Plain[C],
      pd: Plain[D],
      pe: Plain[E]
  ) =
    lastE applyMixed (as, bs, (_, b) => cs(b), (_, _, c) => ds(c), (_, _, _, d) => es(d))

  def apply[T[_]: Mappable, A, B, C, D, E, F](
      as: => Mixed[A, T],
      bs: A => Mixed[B, T],
      cs: B => Mixed[C, T],
      ds: C => Mixed[D, T],
      es: D => Mixed[E, T],
      fs: E => Mixed[F, T]
  )(using
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
      pf: Plain[F]
  ) =
    lastF applyMixed (as, bs, (_, b) => cs(b), (_, _, c) => ds(c), (_, _, _, d) => es(d),
    (_, _, _, _, e) => fs(e))

  def apply[T[_]: Mappable, A, B, C, D, E, F, G](
      as: => Mixed[A, T],
      bs: A => Mixed[B, T],
      cs: B => Mixed[C, T],
      ds: C => Mixed[D, T],
      es: D => Mixed[E, T],
      fs: E => Mixed[F, T],
      gs: F => Mixed[G, T]
  )(using
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
      pg: Plain[G]
  ) =
    lastG applyMixed (as, bs, (_, b) => cs(b), (_, _, c) => ds(c), (_, _, _, d) => es(d),
    (_, _, _, _, e) => fs(e), (_, _, _, _, _, f) => gs(f))

  /** 
    * Variables limited to the previous two
    * 
    *     A
    *     A => T[B]
    *     (A, B) => T[C]
    *     (B, C) => T[D]
    *     (C, D) => T[E]
    *     (D, E) => T[F]
    *     ...
    * 
    * where T[X] is actually T[X] | X, with X to be "lifted" into T[X]
    */

  def apply[T[_]: Mappable, A, B, C, D](
      as: => Mixed[A, T],
      bs: A => Mixed[B, T],
      cs: (A, B) => Mixed[C, T],
      ds: (B, C) => Mixed[D, T]
  )(using
      ta: ClassTag[A],
      tb: ClassTag[B],
      tc: ClassTag[C],
      td: ClassTag[D],
      pa: Plain[A],
      pb: Plain[B],
      pc: Plain[C],
      pd: Plain[D]
  ) =
    lastD applyMixed (as, bs, cs, (_, b, c) => ds(b, c))

  def apply[T[_]: Mappable, A, B, C, D, E](
      as: => Mixed[A, T],
      bs: A => Mixed[B, T],
      cs: (A, B) => Mixed[C, T],
      ds: (B, C) => Mixed[D, T],
      es: (C, D) => Mixed[E, T]
  )(using
      ta: ClassTag[A],
      tb: ClassTag[B],
      tc: ClassTag[C],
      td: ClassTag[D],
      te: ClassTag[E],
      pa: Plain[A],
      pb: Plain[B],
      pc: Plain[C],
      pd: Plain[D],
      pe: Plain[E]
  ) =
    lastE applyMixed (as, bs, cs, (_, b, c) => ds(b, c), (_, _, c, d) => es(c, d))

  def apply[T[_]: Mappable, A, B, C, D, E, F](
      as: => Mixed[A, T],
      bs: A => Mixed[B, T],
      cs: (A, B) => Mixed[C, T],
      ds: (B, C) => Mixed[D, T],
      es: (C, D) => Mixed[E, T],
      fs: (D, E) => Mixed[F, T]
  )(using
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
      pf: Plain[F]
  ) =
    lastF applyMixed (as, bs, cs, (_, b, c) => ds(b, c), (_, _, c, d) => es(c, d),
    (_, _, _, d, e) => fs(d, e))

  def apply[T[_]: Mappable, A, B, C, D, E, F, G](
      as: => Mixed[A, T],
      bs: A => Mixed[B, T],
      cs: (A, B) => Mixed[C, T],
      ds: (B, C) => Mixed[D, T],
      es: (C, D) => Mixed[E, T],
      fs: (D, E) => Mixed[F, T],
      gs: (E, F) => Mixed[G, T]
  )(using
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
      pg: Plain[G]
  ) =
    lastG applyMixed (as, bs, cs, (_, b, c) => ds(b, c), (_, _, c, d) => es(c, d),
    (_, _, _, d, e) => fs(d, e), (_, _, _, _, e, f) => gs(e, f))

  /** 
    * Standard "readable" form: variables are limited to the previous three
    * 
    *     A
    *     A => T[B]
    *     (A, B) => T[C]
    *     (A, B, C) => T[D]
    *     (B, C, D) => T[E]
    *     (C, D, E) => T[F]
    *     ...
    * 
    * where T[X] is actually T[X] | X, with X to be "lifted" into T[X]
    */

  def apply[T[_]: Mappable, A, B, C, D, E](
      as: => Mixed[A, T],
      bs: A => Mixed[B, T],
      cs: (A, B) => Mixed[C, T],
      ds: (A, B, C) => Mixed[D, T],
      es: (B, C, D) => Mixed[E, T]
  )(using
      ta: ClassTag[A],
      tb: ClassTag[B],
      tc: ClassTag[C],
      td: ClassTag[D],
      te: ClassTag[E],
      pa: Plain[A],
      pb: Plain[B],
      pc: Plain[C],
      pd: Plain[D],
      pe: Plain[E]
  ) =
    lastE applyMixed (as, bs, cs, ds, (_, b, c, d) => es(b, c, d))

  def apply[T[_]: Mappable, A, B, C, D, E, F](
      as: => Mixed[A, T],
      bs: A => Mixed[B, T],
      cs: (A, B) => Mixed[C, T],
      ds: (A, B, C) => Mixed[D, T],
      es: (B, C, D) => Mixed[E, T],
      fs: (C, D, E) => Mixed[F, T]
  )(using
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
      pf: Plain[F]
  ) =
    lastF applyMixed (as, bs, cs, ds, (_, b, c, d) => es(b, c, d),
    (_, _, c, d, e) => fs(c, d, e))

  def apply[T[_]: Mappable, A, B, C, D, E, F, G](
      as: => Mixed[A, T],
      bs: A => Mixed[B, T],
      cs: (A, B) => Mixed[C, T],
      ds: (A, B, C) => Mixed[D, T],
      es: (B, C, D) => Mixed[E, T],
      fs: (C, D, E) => Mixed[F, T],
      gs: (D, E, F) => Mixed[G, T]
  )(using
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
      pg: Plain[G]
  ) =
    lastG applyMixed (as, bs, cs, ds, (_, b, c, d) => es(b, c, d),
    (_, _, c, d, e) => fs(c, d, e), (_, _, _, d, e, f) => gs(d, e, f))

  /** 
    * Different last: the last function has several parameters
    * 
    *     A
    *     A => T[B]
    *     B => T[C]
    *     C => T[D]
    *      ...
    *     (D, E, F) => T[G]
    * 
    * where T[X] is actually T[X] | X, with X to be "lifted" into T[X]
    */

  def apply[T[_]: Mappable, A, B, C, D](
      as: => Mixed[A, T],
      bs: A => Mixed[B, T],
      cs: B => Mixed[C, T],
      ds: (A, B, C) => Mixed[D, T]
  )(using
      ta: ClassTag[A],
      tb: ClassTag[B],
      tc: ClassTag[C],
      td: ClassTag[D],
      pa: Plain[A],
      pb: Plain[B],
      pc: Plain[C],
      pd: Plain[D]
  ) =
    lastD applyMixed (as, bs, (_, b) => cs(b), ds)

  /** 
    * Simplified last: the last function has one parameter
    * 
    *     A
    *     A => T[B]
    *     (A, B) => T[C]
    *     (B, C, D) => T[D]
    *      ...
    *     F => T[G]
    * 
    * where T[X] is actually T[X] | X, with X to be "lifted" into T[X]
    */

  def apply[T[_]: Mappable, A, B, C](
      as: => Mixed[A, T],
      bs: => Mixed[B, T],
      cs: B => Mixed[C, T]
  )(using ta: ClassTag[A], tb: ClassTag[B], tc: ClassTag[C], pa: Plain[A], pb: Plain[B], pc: Plain[C]) =
    lastC applyMixed (as, _ => bs, (_, b) => cs(b))

  def apply[T[_]: Mappable, A, B, C, D](
      as: => Mixed[A, T],
      bs: A => Mixed[B, T],
      cs: (A, B) => Mixed[C, T],
      ds: C => Mixed[D, T]
  )(using
      ta: ClassTag[A],
      tb: ClassTag[B],
      tc: ClassTag[C],
      td: ClassTag[D],
      pa: Plain[A],
      pb: Plain[B],
      pc: Plain[C],
      pd: Plain[D]
  ) =
    lastD applyMixed (as, bs, cs, (_, _, c) => ds(c))

  def apply[T[_]: Mappable, A, B, C, D, E](
      as: => Mixed[A, T],
      bs: A => Mixed[B, T],
      cs: (A, B) => Mixed[C, T],
      ds: (A, B, C) => Mixed[D, T],
      es: D => Mixed[E, T]
  )(using
      ta: ClassTag[A],
      tb: ClassTag[B],
      tc: ClassTag[C],
      td: ClassTag[D],
      te: ClassTag[E],
      pa: Plain[A],
      pb: Plain[B],
      pc: Plain[C],
      pd: Plain[D],
      pe: Plain[E]
  ) =
    lastE applyMixed (as, bs, cs, ds, (_, _, _, d) => es(d))

  /** 
    * Various others: these are limited only by the ability of the type system to distinguish between them,
    * i.e. to avoid "typing collisions"
    * 
    */

  def apply[A, B, C](
      as: IndexedSeq[A],
      bs: A => IndexedSeq[B],
      cs: (A, B) => C
  )(using ta: ClassTag[A], tb: ClassTag[B], tc: ClassTag[C], pa: Plain[A], pb: Plain[B], pc: Plain[C]) =
    lastC applyMixed (as, bs, cs)

  def apply[T[_]: Mappable, A, B, C, D](
      as: => Mixed[A, T],
      bs: => Mixed[B, T],
      cs: (A, B) => Mixed[C, T],
      ds: C => Mixed[D, T]
  )(using
      ta: ClassTag[A],
      tb: ClassTag[B],
      tc: ClassTag[C],
      td: ClassTag[D],
      pa: Plain[A],
      pb: Plain[B],
      pc: Plain[C],
      pd: Plain[D]
  ) =
    val allPlainTypes = pa.isPlain && pb.isPlain && pc.isPlain && pd.isPlain
    lastD applyMixed (as, _ => bs, cs, (_, _, c) => ds(c))

  def apply[A, B, C, D](
      as: IndexedSeq[A],
      bs: A => IndexedSeq[B],
      cs: B => IndexedSeq[C],
      ds: (A, B, C) => IndexedSeq[D]
  )(using ta: ClassTag[A], tb: ClassTag[B], tc: ClassTag[C], td: ClassTag[D], pa: Plain[A], pb: Plain[B], pc: Plain[C], pd: Plain[D]) =
    lastD applyMixed (as, bs, (_, b) => cs (b), ds)

  def apply[A, B, C, D](
      as: IndexedSeq[A],
      bs: A => IndexedSeq[B],
      cs: (A, B) => Seq[C],
      ds: (A, B, C) => D
  )(using ta: ClassTag[A], tb: ClassTag[B], tc: ClassTag[C], td: ClassTag[D], pa: Plain[A], pb: Plain[B], pc: Plain[C], pd: Plain[D]) =
    lastD applyMixed (as, bs, (a, b) => cs (a, b).toIndexedSeq, ds)

  def apply[T[_]: Mappable, A, B, C, D, E](
      as: => Mixed[A, T],
      bs: => Mixed[B, T],
      cs: => Mixed[C, T],
      ds: (A, B, C) => Mixed[D, T],
      es: D => Mixed[E, T]
  )(using
      ta: ClassTag[A],
      tb: ClassTag[B],
      tc: ClassTag[C],
      td: ClassTag[D],
      te: ClassTag[E],
      pa: Plain[A],
      pb: Plain[B],
      pc: Plain[C],
      pd: Plain[D],
      pe: Plain[E]
  ) =
    lastE applyMixed (as, _ => bs, (_, _) => cs, ds, (_, _, _, d) => es(d))

  def apply[T[_]: Mappable, A, B, C, D, E, F](
      as: => Mixed[A, T],
      bs: => Mixed[B, T],
      cs: B => Mixed[C, T],
      ds: => Mixed[D, T],
      es: (C, D) => Mixed[E, T],
      fs: => Mixed[F, T]
  )(using
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
      pf: Plain[F]
  ) =
    lastF applyMixed (as, _ => bs, (_, b) => cs(b), (_, _, _) => ds, (_, _, c, d) => es(c, d),
    (_, _, _, _, _) => fs)

  def apply[T[_]: Mappable, A, B, C, D, E, F, G](
      as: MixedThk[A, T],
      bs: MixedThk[B, T],
      cs: MixedThk[C, T],
      ds: MixedThk[D, T],
      es: MixedThk[E, T],
      fs: (A, B, C, D, E) => Mixed[F, T],
      gs: MixedThk[G, T]
  )(using
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
      pg: Plain[G]
  ) =
    lastG applyMixed (as(), _ =>
      bs(), (_, _) => cs(), (_, _, _) => ds(), (_, _, _, _) => es(), fs, (_, _, _, _, _, _) => gs())

  def apply[T[_]: Mappable, A, B, C, D, E, F, G](
      as: => Mixed[A, T],
      bs: => Mixed[B, T],
      cs: => Mixed[C, T],
      ds: => Mixed[D, T],
      es: => Mixed[E, T],
      fs: (B, C, D, E) => Mixed[F, T],
      gs: => Mixed[G, T]
  )(using
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
      pg: Plain[G]
  ) =
    lastG applyMixed (as, _ => bs, (_, _) => cs, (_, _, _) => ds, (_, _, _, _) => es,
    (_, b, c, d, e) => fs(b, c, d, e), (_, _, _, _, _, _) => gs)

/** 
  * Includes less frequently used cases that cause "typing collisions" if included in `sequence`
  * 
  */

object sequenceAlt:

  def apply[T[_]: Mappable, A, B, C](
      as: => Mixed[A, T],
      bs: A => Mixed[B, T],
      cs: (A, B) => Mixed[C, T]
  )(using ta: ClassTag[A], tb: ClassTag[B], tc: ClassTag[C], pa: Plain[A], pb: Plain[B], pc: Plain[C]) =
    lastC applyMixed (as, bs, cs)

  def apply[T[_]: Mappable, A, B, C, D](a: => A, bs: A => B, cs: B => C, ds: C => D)(using
      ta: ClassTag[A],
      tb: ClassTag[B],
      tc: ClassTag[C],
      td: ClassTag[D]
  ) =
    lastD applyMixed (a, bs(_), (_, b) => cs(b), (_, _, c) => ds(c))

/**
  * This covers different forms (e.g. A => Z, (A, B) => Z and (A, B, C) => Z) that can be converted to the same
  * function type (i.e. (A, B, C) => Z).
  * It works but type inference is not as effective.
  */
object mixedforms:

  def apply[T[_]: Mappable, A, B](as: => MixedFormsTA[T, A], bs: => MixedFormsTB[T, A, B])(using
      ta: ClassTag[A],
      tb: ClassTag[B],
      tma: ClassTag[Mixed[A, T]],
      tmb: ClassTag[Mixed[B, T]],
      pa: Plain[A],
      pb: Plain[B]
  ) =
    val as1 = toStdFormA(as)
    val bs1 = toStdFormB(bs)
    lastB applyMixed (as1, bs1)

  def apply[T[_]: Mappable, A, B, C](
      as: => MixedFormsTA[T, A],
      bs: => MixedFormsTB[T, A, B],
      cs: => MixedFormsTC[T, A, B, C]
  )(using
      ta: ClassTag[A],
      tb: ClassTag[B],
      tc: ClassTag[C],
      tma: ClassTag[Mixed[A, T]],
      tmb: ClassTag[Mixed[B, T]],
      tmc: ClassTag[Mixed[C, T]],
      pa: Plain[A],
      pb: Plain[B],
      pc: Plain[C]
  ) =
    val as1 = toStdFormA(as)
    val bs1 = toStdFormB(bs)
    val cs1 = toStdFormC(cs)
    lastC applyMixed (as1, bs1, cs1)

  def apply[T[_]: Mappable, A, B, C, D](
      as: => MixedFormsTA[T, A],
      bs: => MixedFormsTB[T, A, B],
      cs: => MixedFormsTC[T, A, B, C],
      ds: => MixedFormsTD[T, A, B, C, D]
  )(using
      ta: ClassTag[A],
      tb: ClassTag[B],
      tc: ClassTag[C],
      td: ClassTag[D],
      tma: ClassTag[Mixed[A, T]],
      tmb: ClassTag[Mixed[B, T]],
      tmc: ClassTag[Mixed[C, T]],
      tmd: ClassTag[Mixed[D, T]],
      pa: Plain[A],
      pb: Plain[B],
      pc: Plain[C],
      pd: Plain[D]
  ) =
    val as1 = toStdFormA(as)
    val bs1 = toStdFormB(bs)
    val cs1 = toStdFormC(cs)
    val ds1 = toStdFormD(ds)
    lastD applyMixed (as1, bs1, cs1, ds1)

  def apply[T[_]: Mappable, A, B, C, D, E](
      as: => MixedFormsTA[T, A],
      bs: MixedFormsTB[T, A, B],
      cs: MixedFormsTC[T, A, B, C],
      ds: MixedFormsTD[T, A, B, C, D],
      es: MixedFormsTE[T, A, B, C, D, E]
  )(using
      ta: ClassTag[A],
      tb: ClassTag[B],
      tc: ClassTag[C],
      td: ClassTag[D],
      te: ClassTag[E],
      tma: ClassTag[Mixed[A, T]],
      tmb: ClassTag[Mixed[B, T]],
      tmc: ClassTag[Mixed[C, T]],
      tmd: ClassTag[Mixed[D, T]],
      tme: ClassTag[Mixed[E, T]],
      pa: Plain[A],
      pb: Plain[B],
      pc: Plain[C],
      pd: Plain[D],
      pe: Plain[E]
  ) =
    val as1 = toStdFormA(as)
    val bs1 = toStdFormB(bs)
    val cs1 = toStdFormC(cs)
    val ds1 = toStdFormD(ds)
    val es1 = toStdFormE(es)
    lastE applyMixed (as1, bs1, cs1, ds1, es1)
