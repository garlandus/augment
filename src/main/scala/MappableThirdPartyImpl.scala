package mappablethirdparty

import comprehension._
import mappable._
import shape._

import scala.reflect.ClassTag
import scala.util.Try
import cats.data._
import cats.effect.IO
import cats.Eval
import cats.implicits._
import java.io.IOException
import zio.{ZIO, Console}

/** Third party implementations: Cats Effect, ZIO, etc. */

/** Cats Effect */

given Mappable[cats.effect.IO] with
  import cats.effect.unsafe.implicits.global
  extension [A](a: => A) def unit(): IO[A] = IO.delay(a)
  extension [A](as: IO[A])
    def hasValue(): Boolean = true
    def value(): A = as.unsafeRunSync()
    def result(): Either[Throwable, A] =
      as.attempt.unsafeRunSync()
    def map[B](f: A => B): IO[B] = as.map(f)
    def flatMap[B](f: A => IO[B]): IO[B] = as.flatMap(f)
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
  extension [A](as: Eval[A])
    def hasValue(): Boolean = true
    def value(): A = as.value
    def result(): Either[Throwable, A] = Right(as.value)
    def map[B](f: A => B): Eval[B] = as.map(f)
    def flatMap[B](f: A => Eval[B]): Eval[B] = as.flatMap(f)

type CatsReaderX = [F[_]] =>> [A] =>> [B] =>> cats.data.Kleisli[F, A, B]

given [S]: Mappable[CatsReaderX[cats.Id][S]] with
  override def isDelayed: Boolean = false
  extension [A](a: => A) def unit(): Reader[S, A] = Reader(s => a)
  extension [A](as: Reader[S, A])
    def hasValue(): Boolean = true
    def value(): A = ???
    def result(): Either[Throwable, A] = Right(value())
    def map[B2](f: A => B2): Reader[S, B2] = as.map(f)
    def flatMap[B2](f: A => Reader[S, B2]): Reader[S, B2] = as.flatMap(f)

type CatsWriterX = [L] =>> [A] =>> cats.data.Writer[L, A]

given [L: cats.kernel.Monoid]: Mappable[CatsWriterX[L]] with
  override def isDelayed: Boolean = false
  extension [A](a: => A)
    def unit(): Writer[L, A] =
      WriterT(cats.kernel.Monoid.empty, a)
  extension [A](as: Writer[L, A])
    def hasValue(): Boolean = true
    def value(): A = as.value
    def result(): Either[Throwable, A] = Right(as.value)
  extension [A](bs: Writer[L, A])
    def map[B2](f: A => B2): Writer[L, B2] = bs.map(f)
    def flatMap[B2](f: A => Writer[L, B2]): Writer[L, B2] = bs.flatMap(f)
  override def product[A, B](as: Writer[L, A], bs: Writer[L, B]): Writer[L, (A, B)] =
    Writer(as.run._1.combine(bs.run._1), (as.value, bs.value))

type StateX = [S] =>> [A] =>> cats.data.State[S, A]

given [S]: Mappable[StateX[S]] with
  override def isDelayed: Boolean = false
  extension [A](a: => A) def unit(): State[S, A] = State((_, a))
  extension [A](as: State[S, A])
    def hasValue(): Boolean = true
    def value(): A = ???
    def result(): Either[Throwable, A] = Right(value())
    def map[B2](f: A => B2): State[S, B2] = as.map(f)
    def flatMap[B2](f: A => State[S, B2]): State[S, B2] = as.flatMap(f)
  override def product[A, B](as: State[S, A], bs: State[S, B]): State[S, (A, B)] =
    as.flatMap(a => bs.map(b => (a, b)))

/** ZIO */

type zIOX = [R] =>> [E] =>> [A] =>> zio.ZIO[R, E, A]
type zIO[A] = ZIO[Any, IOException, A]
type zUIO[A] = ZIO[Any, Nothing, A]

type zIO1 = [A] =>> ZIO[Any, IOException, A]
type zIO2 = [A] =>> ZIO[Any, IOException | NumberFormatException, A]
type zIOE = [A] =>> ZIO[Any, Exception, A]
type zIOW[A] = ZIO[Any, Exception, A]

given [R, E]: Mappable[zIOX[R][E]] with
  extension [A](a: => A) def unit(): ZIO[R, E, A] =
    ZIO.attempt(a).asInstanceOf[ZIO[R, E, A]]

  extension [A](as: ZIO[R, E, A]) def hasValue(): Boolean = true
  extension [A](as: ZIO[R, E, A])
    def value(): A =
      val res = zio.Unsafe.unsafe(implicit u => zio.Runtime.default.unsafe.run(as.asInstanceOf[ZIO[Any, E, A]]))
      res.toEither match
        case Right(a) => a
        case Left(e)  => throw e
    
    def result(): Either[E, A] =
      val res = zio.Unsafe.unsafe(implicit u => zio.Runtime.default.unsafe.run(as.asInstanceOf[ZIO[Any, E, A]]))
      res.either.value()

    def alternatives(): E | A =
      val res: ZIO[Any, E, A] =
        zio.Unsafe.unsafe(implicit u => zio.Runtime.default.unsafe.run(as.asInstanceOf[ZIO[Any, E, A]]))
      val io = res.fold(failure => failure, success => success)
      io.value()

  extension [A](as: () => ZIO[R, E, A] | A)
    override def unitFromThunk()(using ClassTag[A]): ZIO[R, E, A] =
      val res = 
        ZIO.attempt (
          as() match
            case a: A => a
            case _ =>
              val res = as().asInstanceOf[ZIO[R, E, A]].alternatives()
              res match
                case a1: A => a1
                case _ => throw res.asInstanceOf[Throwable] 
        )
      res.asInstanceOf[ZIO[R, E, A]]

  extension [A](as: ZIO[R, E, A])
    def map[B](f: A => B): ZIO[R, E, B] = as.map(f)
    def flatMap[B](f: A => ZIO[R, E, B]): ZIO[R, E, B] = as.flatMap(f)
  override def product[A, B](as: ZIO[R, E, A], bs: ZIO[R, E, B]): ZIO[R, E, (A, B)] =
    as.zipPar(bs)
  extension [A](as: ZIO[R, E, A])
    override def retry(n: Int): ZIO[R, E, A] =
      as.retryN(n)

/** Eitherish: collections broadly similar to Either */

type ZioE[E, A] = ZIO[Any, E, A]

given Eitherish[ZioE] with
  val tag = "ZIO"
  extension [E, A](e: E) def toLeft() = ZIO.fail(e)
  extension [E, A](a: A) def toRight() = ZIO.succeed(a)
  extension [E, A](x: ZioE[E, A]) def asEither()(using ClassTag[A]): Either[E, A] = ???
  extension [E, A](x: Either[E, A]) def fromEither(): ZioE[E, A] = ???

given EitherishPair[ZioE, ZioE] with
  val tag = "ZioE / ZioE"
  extension [E, A](x: ZioE[E, A]) def asEitherP()(using ClassTag[A]) = ???
  extension [E, A](x: Either[E, A])
    def fromEitherP(): ZioE[E, A] =
      ZIO.fromEither(x)

/** Monad transformers */

import cats.Functor
import cats.Monad
given [F[_]: Monad, E]: ContainerPair[F, EitherX[E]] with
  def tag = "?/Either"
  def areSame(): Boolean = false

  override def applyTransformerPlain[Z, A](as: F[EitherX[E][A]], f: A => Z) =
    val res =
      for a <- EitherT(as)
      yield f(a)
    res.value

  override def applyTransformer[Z, A](as: F[EitherX[E][A]], f: A => F[EitherX[E][Z]]) =
    val res =
      for
        a <- EitherT(as)
        r <- EitherT(f(a))
      yield r
    res.value

  override def applyTransformerPlain[Z, A, B](as: F[EitherX[E][A]], bs: F[EitherX[E][B]], f: (A, B) => Z) =
    val res =
      for
        a <- EitherT(as)
        b <- EitherT(bs)
      yield f(a, b)
    res.value

  override def applyTransformer[Z, A, B](as: F[EitherX[E][A]], bs: F[EitherX[E][B]], f: (A, B) => F[EitherX[E][Z]]) =
    val res =
      for
        a <- EitherT(as)
        b <- EitherT(bs)
        r <- EitherT(f(a, b))
      yield r
    res.value

  override def applyTransformerPlain[Z, A, B, C](
      as: F[EitherX[E][A]],
      bs: F[EitherX[E][B]],
      cs: F[EitherX[E][C]],
      f: (A, B, C) => Z
  ) =
    val res =
      for
        a <- EitherT(as)
        b <- EitherT(bs)
        c <- EitherT(cs)
      yield f(a, b, c)
    res.value

  override def applyTransformer[Z, A, B, C](
      as: F[EitherX[E][A]],
      bs: F[EitherX[E][B]],
      cs: F[EitherX[E][C]],
      f: (A, B, C) => F[EitherX[E][Z]]
  ) =
    val res =
      for
        a <- EitherT(as)
        b <- EitherT(bs)
        c <- EitherT(cs)
        r <- EitherT(f(a, b, c))
      yield r
    res.value

given [F[_]: Monad]: ContainerPair[F, Option] with
  def tag = "?/Option"
  def areSame(): Boolean = false

  override def applyTransformerPlain[Z, A](as: F[Option[A]], f: A => Z) =
    val res =
      for a <- OptionT(as)
      yield f(a)
    res.value

  override def applyTransformer[Z, A](as: F[Option[A]], f: A => F[Option[Z]]) =
    val res =
      for
        a <- OptionT(as)
        r <- OptionT(f(a))
      yield r
    res.value

  override def applyTransformerPlain[Z, A, B](as: F[Option[A]], bs: F[Option[B]], f: (A, B) => Z) =
    val res =
      for
        a <- OptionT(as)
        b <- OptionT(bs)
      yield f(a, b)
    res.value

  override def applyTransformer[Z, A, B](as: F[Option[A]], bs: F[Option[B]], f: (A, B) => F[Option[Z]]) =
    val res =
      for
        a <- OptionT(as)
        b <- OptionT(bs)
        r <- OptionT(f(a, b))
      yield r
    res.value

  override def applyTransformerPlain[Z, A, B, C](
      as: F[Option[A]],
      bs: F[Option[B]],
      cs: F[Option[C]],
      f: (A, B, C) => Z
  ) =
    val res =
      for
        a <- OptionT(as)
        b <- OptionT(bs)
        c <- OptionT(cs)
      yield f(a, b, c)
    res.value

  override def applyTransformer[Z, A, B, C](
      as: F[Option[A]],
      bs: F[Option[B]],
      cs: F[Option[C]],
      f: (A, B, C) => F[Option[Z]]
  ) =
    val res =
      for
        a <- OptionT(as)
        b <- OptionT(bs)
        c <- OptionT(cs)
        r <- OptionT(f(a, b, c))
      yield r
    res.value
