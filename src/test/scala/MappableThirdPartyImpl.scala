package mappablethirdparty

import comprehension._
import mappable._
import shape._

import cats.data._
import cats.effect.IO
import cats.Eval
import cats.implicits._
import com.google.common.collect._
import java.io.IOException
import zio.{ZIO, Console}

// Third party implementations: Cats Effect, ZIO, Guava, etc.

// Cats Effect

given Mappable[cats.effect.IO] with
  import cats.effect.unsafe.implicits.global
  extension [A](a: => A) def unit(): IO[A] = IO.delay(a)
  extension [A](as: IO[A]) def hasValue(): Boolean = true
  extension [A](as: IO[A]) def value(): A = as.unsafeRunSync()
  extension [A](as: IO[A]) def map[B](f: A => B): IO[B] = as.map(f)
  extension [A](as: IO[A]) def flatMap[B](f: A => IO[B]): IO[B] = as.flatMap(f)
  override def product[A, B](as: IO[A], bs: IO[B]): IO[(A, B)] =
    (as, bs).parMapN((_, _))
  override def product[A, B, C](as: IO[A], bs: IO[B], cs: IO[C]): IO[(A, B, C)] =
    (as, bs, cs).parMapN((_, _, _))
  extension [A](as: IO[A])
    override def retry(n: Int) =
      as.handleErrorWith: e =>
        if (n > 0) then as.retry(n - 1) else IO.raiseError(e)

given Mappable[cats.Eval] with
  extension [A](a: => A) def unit(): Eval[A] = a.pure[Eval]
  extension [A](as: Eval[A]) def hasValue(): Boolean = true
  extension [A](as: Eval[A]) def value(): A = as.value
  extension [A](as: Eval[A]) def map[B](f: A => B): Eval[B] = as.map(f)
  extension [A](as: Eval[A]) def flatMap[B](f: A => Eval[B]): Eval[B] = as.flatMap(f)

type CatsReaderX = [S] =>> [A] =>> cats.data.Reader[S, A]

given [S]: Mappable[CatsReaderX[S]] with
  override def isDelayed: Boolean = false
  extension [A](a: => A) def unit(): Reader[S, A] = Reader(s => a)
  extension [A](as: Reader[S, A]) def hasValue(): Boolean = true
  extension [A](as: Reader[S, A]) def value(): A = ???
  extension [B1](bs: Reader[S, B1]) def map[B2](f: B1 => B2): Reader[S, B2] = bs.map(f)
  extension [B1](bs: Reader[S, B1]) def flatMap[B2](f: B1 => Reader[S, B2]): Reader[S, B2] = bs.flatMap(f)

type CatsWriterX = [L] =>> [A] =>> cats.data.Writer[L, A]

given [L](using cats.kernel.Semigroup[L]): Mappable[CatsWriterX[L]] with
  override def isDelayed: Boolean = false
  extension [A](a: => A) def unit(): Writer[L, A] = ???
  extension [A](as: Writer[L, A]) def hasValue(): Boolean = true
  extension [A](as: Writer[L, A]) def value(): A = as.value
  extension [B1](bs: Writer[L, B1]) def map[B2](f: B1 => B2): Writer[L, B2] = bs.map(f)
  extension [B1](bs: Writer[L, B1]) def flatMap[B2](f: B1 => Writer[L, B2]): Writer[L, B2] = bs.flatMap(f)
  override def product[A, B](as: Writer[L, A], bs: Writer[L, B]): Writer[L, (A, B)] =
    Writer(as.run._1.combine(bs.run._1), (as.value, bs.value))

type StateX = [S] =>> [A] =>> cats.data.State[S, A]

given [S]: Mappable[StateX[S]] with
  override def isDelayed: Boolean = false
  extension [A](a: => A) def unit(): State[S, A] = State((_, a))
  extension [A](as: State[S, A]) def hasValue(): Boolean = true
  extension [A](as: State[S, A]) def value(): A = ???
  extension [B1](bs: State[S, B1]) def map[B2](f: B1 => B2): State[S, B2] = bs.map(f)
  extension [B1](bs: State[S, B1]) def flatMap[B2](f: B1 => State[S, B2]): State[S, B2] = bs.flatMap(f)
  override def product[A, B](as: State[S, A], bs: State[S, B]): State[S, (A, B)] =
    as.flatMap(a => bs.map(b => (a, b)))

// ZIO

type zIOX = [R] =>> [E] =>> [A] =>> zio.ZIO[R, E, A]
type zIO[A] = ZIO[Any, IOException, A]

given [R, E]: Mappable[zIOX[R][E]] with
  extension [A](a: => A) def unit(): ZIO[R, E, A] = ZIO.succeed(a)
  extension [A](as: ZIO[R, E, A]) def hasValue(): Boolean = true
  extension [A](as: ZIO[R, E, A])
    def value(): A =
      val res = zio.Unsafe.unsafe(implicit u => zio.Runtime.default.unsafe.run(as.asInstanceOf[ZIO[Any, E, A]]))
      res.toEither match
        case Right(a) => a
        case Left(a)  => throw Exception(a.toString())
  extension [A](as: ZIO[R, E, A]) def map[B](f: A => B): ZIO[R, E, B] = as.map(f)
  extension [A](as: ZIO[R, E, A]) def flatMap[B](f: A => ZIO[R, E, B]): ZIO[R, E, B] = as.flatMap(f)
  override def product[A, B](as: ZIO[R, E, A], bs: ZIO[R, E, B]): ZIO[R, E, (A, B)] =
    as.zipPar(bs)
  extension [A](as: ZIO[R, E, A])
    override def retry(n: Int): ZIO[R, E, A] =
      as.retryN(n)

// Guava comprehensions

given ComprehensionB[com.google.common.collect.Table] with
  def irregular[X, Z, A, B]: IrregComprehensionB[Table, X, Z, A, B] =
    (as: Seq[A], bsDep: DepSeqB[A, B], f: (A, B) => X, g: X => Z) =>
      val table: Table[Z, A, B] = HashBasedTable.create()
      as.map: a =>
        bsDep(a).map: b =>
          table.put(g(f(a, b)), a, b)
      table

type GuavaMultiSet[Z, A, B, C] = com.google.common.collect.Multiset[Z]

given ComprehensionC[GuavaMultiSet] with
  def irregular[X, Z, A, B, C]: IrregComprehensionC[GuavaMultiSet, X, Z, A, B, C] =
    (as: Seq[A], bsDep: DepSeqB[A, B], csDep: DepSeqC[A, B, C], f: (A, B, C) => X, g: X => Z) =>
      val multiSet: Multiset[Z] = HashMultiset.create()
      as.flatMap: a =>
        bsDep(a).flatMap: b =>
          csDep(a, b).map: c =>
            multiSet.add(g(f(a, b, c)))
      multiSet
