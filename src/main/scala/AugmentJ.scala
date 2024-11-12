package augmented

import basicdef._
import comprehension.given
import mappable._
import mappable.given
import mappablethirdparty.given
import multiarray._
import shape._
import util._
import util.JavaUtil.{Pair, Triple}

import collection.JavaConverters._
import java.util.stream._
import java.util.function.Supplier
import zio._

def tupleJPair[A, B] = augment((a: A, b: B) => Pair(a, b))
def tupleJTriple[A, B, C] = augment((a: A, b: B, c: C) => Triple(a, b, c))

/** used from Java */
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

  def select[A, B](as: JList[A], bs: JDepSeq[A, B], phi: (A, B) => Boolean) =
    tupleJPair(as, bs, phi)

  def select[A, B, C](as: JList[A], bs: JDepSeq[A, B], cs: JDepSeq[B, C]) =
    tupleJTriple applyJ (as, bs, cs)

  def select[A, B, C](as: JList[A], bs: JDepSeq[A, B], cs: JDepSeq[B, C], phi: (A, B, C) => Boolean) =
    tupleJTriple applyJ (as, bs, cs, phi)

  def select[A, B](as: BaseStream[A, ?], bsDep: A => BaseStream[B, ?]) =
    val as1 = streamToLazyList(as)
    val bsDep1 = (a: A) => streamToLazyList(bsDep(a))
    tupleJPair(as1, bsDep1).asJava

  def select[A, B, C](as: BaseStream[A, ?], bs: BaseStream[B, ?], cs: BaseStream[C, ?], phi: (A, B, C) => Boolean) =
    val (as1, bs1, cs1) = (streamToLazyList(as), streamToLazyList(bs), streamToLazyList(cs))
    tupleJTriple(as1, (a: A) => bs1, (b: B) => cs1, phi).asJava

  def select[A, B, C](as: BaseStream[A, ?], bsDep: A => BaseStream[B, ?], csDep: B => BaseStream[C, ?]) =
    val as1 = streamToLazyList(as)
    val bsDep1 = (a: A) => streamToLazyList(bsDep(a))
    val csDep1 = (b: B) => streamToLazyList(csDep(b))
    tupleJTriple(as1, bsDep1, csDep1, (a, b, c) => true).asJava

  def select[A, B, C](
      as: BaseStream[A, ?],
      bsDep: A => BaseStream[B, ?],
      csDep: B => BaseStream[C, ?],
      phi: (A, B, C) => Boolean
  ) =
    val as1 = streamToLazyList(as)
    val bsDep1 = (a: A) => streamToLazyList(bsDep(a))
    val csDep1 = (b: B) => streamToLazyList(csDep(b))
    tupleJTriple(as1, bsDep1, csDep1, phi).asJava

  def count[A, B, C](as: JList[A], bsDep: JDepSeq[A, B], csDep: JDepSeq[B, C]) =
    (tupleJTriple applyJ (as, bsDep, csDep)).size()

  def count[A, B, C](as: BaseStream[A, ?], bsDep: JDepStream[A, B], csDep: JDepStream[B, C]) =
    (select(as, bsDep, csDep, (a, b, c) => true)).size()

  def queue[B](as: java.lang.Runnable, b: => B, cs: java.util.function.Consumer[B]) =
    MappableW(augmented.queue(as.run(), _ => b, cs.accept(_)))

  def queue[A, B, C](a: => A, b: => B, cs: B => C) =
    MappableW(augmented.queue(a, _ => b, cs))

  def queue[A, B, C](a: => A, bs: A => B, cs: B => C) =
    MappableW(augmented.queue(a, bs(_), cs))

  def queue[B, C, D](as: java.lang.Runnable, b: => B, cs: B => C, ds: C => D) =
    MappableW(augmented.queue(as.run(), _ => b, (a, b) => cs(b), (a, b, c) => ds(c)))

  def queue[A, B, C, D](a: => A, b: => B, cs: B => C, ds: C => D) =
    MappableW(augmented.queue(a, _ => b, (a, b) => cs(b), (a, b, c) => ds(c)))

  def seqContains[A](s: IndexedSeq[A], a: A) = s.contains(a)

  def zioSucceed[E, A](a: A): ZIO[Any, E, A] = ZIO.succeed(a).asInstanceOf[ZIO[Any, E, A]]
  def zioSucceed[E, A](a: () => A): ZIO[Any, E, A] = ZIO.succeed(a()).asInstanceOf[ZIO[Any, E, A]]
  def zioFail[E, A](e: E): ZIO[Any, E, A] = ZIO.fail(e)
  def zioOrElseFail[E, A](as: ZIO[Any, Throwable, A], e: E): ZIO[Any, E, A] = as.orElseFail(e)
  def zioFailIf[E](pred: () => Boolean, e: E): ZIO[Any, E, Unit] = if pred() then ZIO.succeed(()) else ZIO.fail(e)

  def zioAttempt[A](a: () => A): Task[A] = ZIO.attempt(a())
  def zioAttempt[E, A](a: () => A, e: E): ZIO[Any, E, A] = ZIO.attempt(a()).orElseFail(e)

  def zioFromEither[E, A](f: Supplier[Either[E, A]]) = ZIO.from(f.get())
  def zioFrom[E, A](eith: => Either[E, A]) = ZIO.from(eith)

  def zioPrintLine(s: String) = Console.printLine(s)
  def zioReadLine(s: String) = Console.readLine(s)
  def zioRepeat[R, E, A](io: ZIO[R, E, A], n: Int) = io.repeat(Schedule.recurs(n))

  def zioForeachDiscard[R, E, A, B](as: JList[A], f: A => ZIO[R, E, B]): ZIO[R, E, Unit] =
    ZIO.foreachDiscard(scala.jdk.javaapi.CollectionConverters.asScala(as))(f)
  def zioSleep(n: Int) = ZIO.sleep(n.seconds)

  def zioRefMake[A](a: A): UIO[Ref[A]] = Ref.make(a)
  def zioRefGet[A](r: Ref[A]): UIO[A] = r.get

  def zioRefModify[A, B](r: Ref[A], f: A => util.JavaUtil.Pair[B, A]): UIO[B] = r.modify(a => {
    val pr = f(a); (pr.first, pr.second)
  })

  def value[R, E, A](x: ZIO[R, E, A]) = x.value()
  def result[R, E, A](x: ZIO[R, E, A]) = x.result()

  trait IZioU[A]:
    def io(): IO[Any, A]
    def value(): A

  case class ZIO2[A, E1, E2](zio: IO[E1 | E2, A]) extends IZioU[A]:
    def io(): IO[E1 | E2, A] = zio
    def value(): A = zio.value()
    def result(): Either[E1 | E2, A] = zio.result()

  case class ZIO3[A, E1, E2, E3](zio: IO[E1 | E2 | E3, A]) extends IZioU[A]:
    def io(): IO[E1 | E2 | E3, A] = zio
    def value(): A = zio.value()
    def result(): Either[E1 | E2 | E3, A] = zio.result()

  case class ZIO4[A, E1, E2, E3, E4](zio: IO[E1 | E2 | E3 | E4, A]) extends IZioU[A]:
    def io(): IO[E1 | E2 | E3 | E4, A] = zio
    def value(): A = zio.value()
    def result(): Either[E1 | E2 | E3 | E4, A] = zio.result()

  case class ZIO5[A, E1, E2, E3, E4, E5](zio: IO[E1 | E2 | E3 | E4 | E5, A]) extends IZioU[A]:
    def io(): IO[E1 | E2 | E3 | E4 | E5, A] = zio
    def value(): A = zio.value()
    def result(): Either[E1 | E2 | E3 | E4 | E5, A] = zio.result()

  case class ZIO6[A, E1, E2, E3, E4, E5, E6](zio: IO[E1 | E2 | E3 | E4 | E5 | E6, A]) extends IZioU[A]:
    def io(): IO[E1 | E2 | E3 | E4 | E5 | E6, A] = zio
    def value(): A = zio.value()
    def result(): Either[E1 | E2 | E3 | E4 | E5 | E6, A] = zio.result()

  case class ZIO7[A, E1, E2, E3, E4, E5, E6, E7](zio: IO[E1 | E2 | E3 | E4 | E5 | E6 | E7, A]) extends IZioU[A]:
    def io(): IO[E1 | E2 | E3 | E4 | E5 | E6 | E7, A] = zio
    def value(): A = zio.value()
    def result(): Either[E1 | E2 | E3 | E4 | E5 | E6 | E7, A] = zio.result()

  def zioFrom[E1, A] (as: () => Either[E1, A])                        = () => ZIO.from(as()) 
  def zioFrom[E1, A, B] (bs: A => Either[E1, B])                      = (a: A) => ZIO.from(bs (a))
  def zioFrom[E1, A, B, C] (cs: (A, B) => Either[E1, C])              = (a: A, b: B) => ZIO.from(cs (a, b))
  def zioFrom[E1, A, B, C, D] (ds: (A, B, C) => Either[E1, D])        = (a: A, b: B, c: C) => ZIO.from(ds (a, b, c))
  def zioFrom[E1, A, B, C, D, E] (es: (A, B, C, D) => Either[E1, E])  = (a: A, b: B, c: C, d: D) => ZIO.from(es (a, b, c, d))   
  def zioFrom[E1, A, B, C, D, E, F] (fs: (A, B, C, D, E) => Either[E1, F]): (A, B, C, D, E) => ZIO[Any, E1, F] =
    (a: A, b: B, c: C, d: D, e: E) => ZIO.from(fs (a, b, c, d, e)) 
  def zioFrom[E1, A, B, C, D, E, F, G] (gs: (A, B, C, D, E, F) => Either[E1, G]) =
    (a: A, b: B, c: C, d: D, e: E, f: F) => ZIO.from(gs (a, b, c, d, e, f)) 

  def stdSequenceEith[A, B, E1, E2](as: () => Either[E1, A], bs: A => Either[E2, B]): ZIO2[B, E1, E2] =
    sequence(
      zioFrom(as),
      zioFrom(bs)
    )

  def stdSequenceEith[A, B, C, E1, E2, E3](
      as: () => Either[E1, A],
      bs: A => Either[E2, B],
      cs: (A, B) => Either[E3, C]
  ): ZIO3[C, E1, E2, E3] =
    sequence(
      zioFrom(as),
      zioFrom(bs),
      zioFrom(cs)
    )

  def sequenceEith[A, B, C, E1, E2, E3](
      as: () => Either[E1, A],
      bs: A => Either[E2, B],
      cs: (A, B) => Either[E3, C]
  ): ZIO3[C, E1, E2, E3] =
    stdSequenceEith(as, bs, cs)

  def stdSequenceEith[A, B, C, D, E1, E2, E3, E4](
      as: () => Either[E1, A],
      bs: A => Either[E2, B],
      cs: (A, B) => Either[E3, C],
      ds: (A, B, C) => Either[E4, D]
  ): ZIO4[D, E1, E2, E3, E4] =
    sequence(
      zioFrom(as),
      zioFrom(bs),
      zioFrom(cs),
      zioFrom(ds)
    )

  def sequenceEith[A, B, C, D, E1, E2, E3, E4](
      as: () => Either[E1, A],
      bs: A => Either[E2, B],
      cs: (A, B) => Either[E3, C],
      ds: (A, B, C) => Either[E4, D]
  ): ZIO4[D, E1, E2, E3, E4] =
    stdSequenceEith(as, bs, cs, ds)

  def sequenceEith[A, B, C, D, E1, E2, E3, E4](
      as: () => Either[E1, A],
      bs: A => Either[E2, B],
      cs: (A, B) => Either[E3, C],
      ds: (B, C) => Either[E4, D]
  ): ZIO4[D, E1, E2, E3, E4] =
    stdSequenceEith(as, bs, cs, (_, b, c) => ds(b, c))

  def stdSequenceEith[A, B, C, D, E, E1, E2, E3, E4, E5](
      as: () => Either[E1, A],
      bs: A => Either[E2, B],
      cs: (A, B) => Either[E3, C],
      ds: (A, B, C) => Either[E4, D],
      es: (A, B, C, D) => Either[E5, E]
  ): ZIO5[E, E1, E2, E3, E4, E5] =
    sequence(
      zioFrom(as),
      zioFrom(bs),
      zioFrom(cs),
      zioFrom(ds),
      zioFrom(es)
    )

  def stdSequenceEith[A, B, C, D, E, F, E1, E2, E3, E4, E5, E6](
      as: () => Either[E1, A],
      bs: A => Either[E2, B],
      cs: (A, B) => Either[E3, C],
      ds: (A, B, C) => Either[E4, D],
      es: (A, B, C, D) => Either[E5, E],
      fs: (A, B, C, D, E) => Either[E6, F]
  ): ZIO6[F, E1, E2, E3, E4, E5, E6] =
    sequence(
      zioFrom(as),
      zioFrom(bs),
      zioFrom(cs),
      zioFrom(ds),
      zioFrom(es),
      zioFrom(fs)
    )

  def sequenceEith[A, B, C, D, E, F, E1, E2, E3, E4, E5, E6](
      as: () => Either[E1, A],
      bs: A => Either[E2, B],
      cs: (A, B) => Either[E3, C],
      ds: (A, B, C) => Either[E4, D],
      es: (A, B, C, D) => Either[E5, E],
      fs: (A, B, C, D, E) => Either[E6, F]
  ): ZIO6[F, E1, E2, E3, E4, E5, E6] =
    stdSequenceEith(as, bs, cs, ds, es, fs)

  def sequenceEith[A, B, C, D, E, F, E1, E2, E3, E4, E5, E6](
      as: () => Either[E1, A],
      bs: A => Either[E2, B],
      cs: (A, B) => Either[E3, C],
      ds: (B, C) => Either[E4, D],
      es: (C, D) => Either[E5, E],
      fs: (D, E) => Either[E6, F]
  ): ZIO6[F, E1, E2, E3, E4, E5, E6] =
    stdSequenceEith(as, bs, cs, (_, b, c) => ds(b, c), (_, _, c, d) => es(c, d), (_, _, _, d, e) => fs(d, e))

  def stdSequenceEith[A, B, C, D, E, F, G, E1, E2, E3, E4, E5, E6, E7](
      as: () => Either[E1, A],
      bs: A => Either[E2, B],
      cs: (A, B) => Either[E3, C],
      ds: (A, B, C) => Either[E4, D],
      es: (A, B, C, D) => Either[E5, E],
      fs: (A, B, C, D, E) => Either[E6, F],
      gs: (A, B, C, D, E, F) => Either[E7, G]
  ): ZIO7[G, E1, E2, E3, E4, E5, E6, E7] =
    sequence(
      zioFrom(as),
      zioFrom(bs),
      zioFrom(cs),
      zioFrom(ds),
      zioFrom(es),
      zioFrom(fs),
      zioFrom(gs)
    )

  def sequenceEith[A, B, C, D, E, E1, E2, E3, E4, E5](
      as: () => Either[E1, A],
      bs: A => Either[E2, B],
      cs: (A, B) => Either[E3, C],
      ds: (A, B, C) => Either[E4, D],
      es: (A, B, C, D) => Either[E5, E]
  ): ZIO5[E, E1, E2, E3, E4, E5] =
    stdSequenceEith(as, bs, cs, ds, es)

  def sequenceEith[A, B, C, D, E, E1, E2, E3, E4, E5](
      as: () => Either[E1, A],
      bs: A => Either[E2, B],
      cs: (A, B) => Either[E3, C],
      ds: (B, C) => Either[E4, D],
      es: (C, D) => Either[E5, E]
  ): ZIO5[E, E1, E2, E3, E4, E5] =
    stdSequenceEith(as, bs, cs, (_, b, c) => ds(b, c), (_, _, c, d) => es(c, d))

  def stdSequence[A, B, E1, E2](as: () => IO[E1, A], bs: A => IO[E2, B]): ZIO2[B, E1, E2] =
    ZIO2(lastB applyStandardForm (as(), bs))

  def sequence[A, B, E1, E2](as: () => IO[E1, A], bs: A => IO[E2, B]): ZIO2[B, E1, E2] =
    stdSequence(as, bs)

  def stdSequence[A, B, C, E1, E2, E3](
      as: () => IO[E1, A],
      bs: A => IO[E2, B],
      cs: (A, B) => IO[E3, C]
  ): ZIO3[C, E1, E2, E3] =
    ZIO3(lastC applyStandardForm (as(), bs, cs))

  def sequence[A, B, C, E1, E2, E3](
      as: () => IO[E1, A],
      bs: A => IO[E2, B],
      cs: (A, B) => IO[E3, C]
  ): ZIO3[C, E1, E2, E3] =
    stdSequence(as, bs, cs)

  def sequence[A, B, C, E1, E2, E3](as: () => IO[E1, A], bs: A => IO[E2, B], cs: B => IO[E3, C]): ZIO3[C, E1, E2, E3] =
    stdSequence(as, bs, (_, b) => cs(b))

  def stdSequence[A, B, C, D, E1, E2, E3, E4](
      as: () => IO[E1, A],
      bs: A => IO[E2, B],
      cs: (A, B) => IO[E3, C],
      ds: (A, B, C) => IO[E4, D]
  ): ZIO4[D, E1, E2, E3, E4] =
    ZIO4(lastD applyStandardForm (as(), bs, cs, ds))

  def sequence[A, B, C, D, E1, E2, E3, E4](
      as: () => IO[E1, A],
      bs: A => IO[E2, B],
      cs: (A, B) => IO[E3, C],
      ds: (A, B, C) => IO[E4, D]
  ): ZIO4[D, E1, E2, E3, E4] =
    stdSequence(as, bs, cs, ds)

  def sequence[A, B, C, D, E1, E2, E3, E4](
      as: () => IO[E1, A],
      bs: A => IO[E2, B],
      cs: B => IO[E3, C],
      ds: C => IO[E4, D]
  ): ZIO4[D, E1, E2, E3, E4] =
    stdSequence(as, bs, (_, b) => cs(b), (_, _, c) => ds(c))

  def stdSequence[A, B, C, D, E, E1, E2, E3, E4, E5](
      as: () => IO[E1, A],
      bs: A => IO[E2, B],
      cs: (A, B) => IO[E3, C],
      ds: (A, B, C) => IO[E4, D],
      es: (A, B, C, D) => IO[E5, E]
  ): ZIO5[E, E1, E2, E3, E4, E5] =
    ZIO5(lastE applyStandardForm (as(), bs, cs, ds, es))

  def sequence[A, B, C, D, E, E1, E2, E3, E4, E5, E6](
      as: () => IO[E1, A],
      bs: A => IO[E2, B],
      cs: (A, B) => IO[E3, C],
      ds: (A, B, C) => IO[E4, D],
      es: (A, B, C, D) => IO[E5, E]
  ): ZIO5[E, E1, E2, E3, E4, E5] =
    stdSequence(as, bs, cs, ds, es)

  def sequence[A, B, C, D, E, E1, E2, E3, E4, E5, E6](
      as: () => IO[E1, A],
      bs: A => IO[E2, B],
      cs: B => IO[E3, C],
      ds: C => IO[E4, D],
      es: D => IO[E5, E]
  ): ZIO5[E, E1, E2, E3, E4, E5] =
    stdSequence(as, bs, (_, b) => cs(b), (_, _, c) => ds(c), (_, _, _, d) => es(d))

  def sequence[A, B, C, D, E, E1, E2, E3, E4, E5, E6](
      as: () => IO[E1, A],
      bs: A => IO[E2, B],
      cs: (A, B) => IO[E3, C],
      ds: (B, C) => IO[E4, D],
      es: (C, D) => IO[E5, E]
  ): ZIO5[E, E1, E2, E3, E4, E5] =
    stdSequence(as, bs, cs, (_, b, c) => ds(b, c), (_, _, c, d) => es(c, d))

  def stdSequence[A, B, C, D, E, F, E1, E2, E3, E4, E5, E6](
      as: () => IO[E1, A],
      bs: A => IO[E2, B],
      cs: (A, B) => IO[E3, C],
      ds: (A, B, C) => IO[E4, D],
      es: (A, B, C, D) => IO[E5, E],
      fs: (A, B, C, D, E) => IO[E6, F]
  ): ZIO6[F, E1, E2, E3, E4, E5, E6] =
    ZIO6(lastF applyStandardForm (as(), bs, cs, ds, es, fs))

  def sequence[A, B, C, D, E, F, E1, E2, E3, E4, E5, E6](
      as: () => IO[E1, A],
      bs: A => IO[E2, B],
      cs: (A, B) => IO[E3, C],
      ds: (A, B, C) => IO[E4, D],
      es: (A, B, C, D) => IO[E5, E],
      fs: (A, B, C, D, E) => IO[E6, F]
  ): ZIO6[F, E1, E2, E3, E4, E5, E6] =
    stdSequence(as, bs, cs, ds, es, fs)

  def sequence[A, B, C, D, E, F, E1, E2, E3, E4, E5, E6](
      as: () => IO[E1, A],
      bs: A => IO[E2, B],
      cs: (A, B) => IO[E3, C],
      ds: (A, B, C) => IO[E4, D],
      es: (B, C, D) => IO[E5, E],
      fs: (C, D, E) => IO[E6, F]
  ): ZIO6[F, E1, E2, E3, E4, E5, E6] =
    stdSequence(as, bs, cs, ds, (_, b, c, d) => es(b, c, d), (_, _, c, d, e) => fs(c, d, e))

  def stdSequence[A, B, C, D, E, F, G, E1, E2, E3, E4, E5, E6, E7](
      as: () => IO[E1, A],
      bs: A => IO[E2, B],
      cs: (A, B) => IO[E3, C],
      ds: (A, B, C) => IO[E4, D],
      es: (A, B, C, D) => IO[E5, E],
      fs: (A, B, C, D, E) => IO[E6, F],
      gs: (A, B, C, D, E, F) => IO[E7, G]
  ): ZIO7[G, E1, E2, E3, E4, E5, E6, E7] =
    ZIO7(lastG applyStandardForm (as(), bs, cs, ds, es, fs, gs))

  def sequence[A, B, C, D, E, F, G, E1, E2, E3, E4, E5, E6, E7](
      as: () => IO[E1, A],
      bs: A => IO[E2, B],
      cs: (A, B) => IO[E3, C],
      ds: (A, B, C) => IO[E4, D],
      es: (A, B, C, D) => IO[E5, E],
      fs: (A, B, C, D, E) => IO[E6, F],
      gs: (A, B, C, D, E, F) => IO[E7, G]
  ): ZIO7[G, E1, E2, E3, E4, E5, E6, E7] =
    stdSequence(as, bs, cs, ds, es, fs, gs)

  def sequence[A, B, C, D, E, F, G, E1, E2, E3, E4, E5, E6, E7](
      as: () => IO[E1, A],
      bs: A => IO[E2, B],
      cs: (A, B) => IO[E3, C],
      ds: (A, B, C) => IO[E4, D],
      es: (B, C, D) => IO[E5, E],
      fs: (C, D, E) => IO[E6, F],
      gs: (D, E, F) => IO[E7, G]
  ): ZIO7[G, E1, E2, E3, E4, E5, E6, E7] =
    stdSequence(
      as,
      bs,
      cs,
      ds,
      (_, b, c, d) => es(b, c, d),
      (_, _, c, d, e) => fs(c, d, e),
      (_, _, _, d, e, f) => gs(d, e, f)
    )
