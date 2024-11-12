package augmented

import augmented._
import augmented.given
import basicdef._
import mappable._
import mappable.given
import mappablethirdparty.given
import shape._

import scala.annotation.targetName
import scala.concurrent._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.reflect.ClassTag
import scala.util.Try

import cats.data.EitherT
import cats.syntax.traverse._

import java.lang.IndexOutOfBoundsException
import java.lang.NumberFormatException
import java.util.NoSuchElementException

/** These extensions allow functions to be augmented by default.
  * (Methods are not affected.)
  */

object Extensions:

  extension [Z: Atomic, A: Atomic, R[_, _], S[_, _]](f: A => Z)

    def apply(as: Seq[A])(using AugmentA[R, S], ClassTag[Z]): R[Z, A] =
      augment(f)(as)

    def apply[T[_]: Mappable](as: => Mixed[A, T])(using AugmentA[R, S], ClassTag[A], ClassTag[Z], Plain[A], Plain[Z]) =
      sequence(as, f)

    infix def andThen[T[_]: Mappable, Y: Atomic](
        g: Z => T[Y]
    )(using AugmentA[R, S], ClassTag[A], ClassTag[Y], ClassTag[Z]) =
      (a: A) => g(f(a))

  extension [T[_]: Applicative, Z: Atomic, A: Atomic, R[_, _], S[_, _]](f: A => T[Z])
    def apply(as: List[A])(using AugmentA[R, S], ClassTag[A], ClassTag[Z], ClassTag[T[Z]]): T[List[Z]] =
      val g = augment(f)
      g(as)

  extension [T[_]: Mappable, Z: Atomic, A: Atomic, R[_, _], S[_, _]](f: A => T[Z])
    def apply(as: => T[A])(using AugmentA[R, S], ClassTag[A], ClassTag[Z], Plain[A], Plain[Z]) =
      sequence(as, f)

  extension [T[_]: Mappable, U[_]: Applicative, Z: Atomic, A: Atomic, R[_, _], S[_, _]](f: A => T[U[Z]])

    def apply(
        as: Nested[T, U][A]
    )(using a: AugmentA[R, S], ta: ClassTag[A], tz: ClassTag[Z], pa: Plain[A], pz: Plain[Z], cp: ContainerPair[T, U]) =
      cp.applyTransformer(as, f)

  extension [E, Z: Atomic, A: Atomic, R[_, _], S[_, _]](f: A => Future[Either[E, Z]])

    def apply(
        as: List[A]
    )(using aa: AugmentA[R, S], ta: ClassTag[A], tz: ClassTag[Future[Z] | Z]): Future[Either[E, List[Z]]] =
      val f1 = (a: A) => f(a)
      val l = as.map(f1)
      l.traverse(EitherT(_)).value

  extension [A, R[_, _], S[_, _]](f: A => Boolean)
    def apply(as: => Seq[A])(using AugmentA[R, S], ClassTag[A]) =
      augment(f)(as)

  extension [Z: Atomic, A: Atomic, R[_, _], S[_, _]](f: A => String)
    def apply[T[_]: Mappable](as: => Mixed[A, T])(using AugmentA[R, S], ClassTag[A], Plain[A]) =
      augment[String, A, R, S](f).applyMixedRect(as)

  extension [Z: Atomic, A: Atomic, B: Atomic, R[_, _, _], S[_, _, _]](f: (A, B) => Z)

    def apply(
        as: Seq[A],
        bs: Seq[B]
    )(using AugmentB[R, S], ClassTag[A], ClassTag[B], ClassTag[Z], Plain[A], Plain[B], Plain[Z]) =
      augment(f)(as, bs)

    def apply(
        as: Seq[A],
        bsGen: DepSeqB[A, B]
    )(using AugmentB[R, S], ClassTag[A], ClassTag[B], ClassTag[Z], Plain[A], Plain[B], Plain[Z]) =
      augment(f)(as, bsGen)

    def applyMixed[T[_]: Mappable](
        as: => Mixed[A, T],
        bs: => Mixed[B, T]
    )(using AugmentB[R, S], ClassTag[A], ClassTag[B], ClassTag[Z], Plain[A], Plain[B], Plain[Z]) =
      sequence(as, bs, f)

    def apply[T[_]: Mappable](
        as: => Mixed[A, T],
        bs: => Mixed[B, T]
    )(using AugmentB[R, S], ClassTag[A], ClassTag[B], ClassTag[Z], Plain[A], Plain[B], Plain[Z]): T[Z] =
      applyMixed(as, bs)

    def apply[T[_]: Mappable, U[_]: Applicative](as: Nested[T, U][A], bs: Nested[T, U][B])(using
        cp: ContainerPair[T, U]
    ) =
      cp.applyTransformerPlain(as, bs, f)

  /** e.g. String */
  extension [Z: Atomic, A: Atomic, B: Atomic, R[_, _, _], S[_, _, _]](f: (A, B) => Comparable[Z])
    def apply[T[_]: Mappable](as: => Mixed[A, T], bs: => Mixed[B, T])(using
        AugmentB[R, S],
        ClassTag[A],
        ClassTag[B],
        ClassTag[Z],
        ClassTag[Comparable[Z]],
        Plain[A],
        Plain[B],
        Plain[Z],
        Plain[Comparable[Z]]
    ) =
      augment(f)(as, bs)

  extension [T[_]: Mappable, Z: Atomic, A: Atomic, B: Atomic, R[_, _, _], S[_, _, _]](f: (A, B) => T[Z])
    def apply(
        as: => Mixed[A, T],
        bs: => Mixed[B, T]
    )(using AugmentB[R, S], ClassTag[A], ClassTag[B], ClassTag[Z], Plain[A], Plain[B], Plain[Z]) =
      sequence(as, bs, f)

  extension [T[_]: Mappable, U[_]: Applicative, Z: Atomic, A: Atomic, B: Atomic, R[_, _, _], S[_, _, _]](
      f: (A, B) => Nested[T, U][Z]
  )
    def apply(as: Nested[T, U][A], bs: Nested[T, U][B])(using
        a: AugmentB[R, S],
        ta: ClassTag[A],
        tb: ClassTag[B],
        tz: ClassTag[Z],
        pa: Plain[A],
        pb: Plain[B],
        pz: Plain[Z],
        cp: ContainerPair[T, U]
    ) =
      cp.applyTransformer(as, bs, f)

  extension [A: Atomic, R[_, _, _], S[_, _, _]](f: (A, A) => A)

    def apply(as: Seq[A])(using AugmentB[R, S], ClassTag[A]) =
      augment(f)(as)

    def apply[T[_]: Applicative](as: Seq[T[A]])(using AugmentB[R, S], ClassTag[A]) =
      augment(f)(as)

    def apply[T[_]: Mappable](as1: => Mixed[A, T], as2: => Mixed[A, T])(using AugmentB[R, S], ClassTag[A], Plain[A]) =
      f.applyMixed(as1, as2)

    def apply[T[_]: Mappable, U[_]: Applicative](as1: Nested[T, U][A], as2: Nested[T, U][A])(using
        cp: ContainerPair[T, U]
    ) =
      cp.applyTransformerPlain(as1, as2, f)

  extension [Z, A, B, C, R[_, _, _, _], S[_, _, _, _]](f: (A, B, C) => Z)

    def apply(as: Seq[A], bs: Seq[B], cs: Seq[C])(using
        AugmentC[R, S],
        ClassTag[A],
        ClassTag[B],
        ClassTag[C],
        ClassTag[Z],
        Plain[A],
        Plain[B],
        Plain[C],
        Plain[Z]
    ) =
      augment(f)(as, bs, cs)

    def apply(as: Seq[A], bs: GenSeqB[A, B], cs: GenSeqC[A, B, C])(using
        AugmentC[R, S],
        ClassTag[A],
        ClassTag[B],
        ClassTag[C],
        ClassTag[Z],
        Plain[A],
        Plain[B],
        Plain[C],
        Plain[Z]
    ) =
      augment(f)(as, bs, cs)

    def apply[T[_]: Mappable](as: T[A], bs: DepTB[T, A, B], cs: DepTC[T, A, B, C])(using
        AugmentC[R, S],
        ClassTag[A],
        ClassTag[B],
        ClassTag[C],
        ClassTag[Z],
        Plain[A],
        Plain[B],
        Plain[C],
        Plain[Z]
    ) =
      augment(f)(as, bs, cs)

    def apply[T[_]: Mappable, U[_]: Applicative](as: Nested[T, U][A], bs: Nested[T, U][B], cs: Nested[T, U][C])(using
        a: AugmentC[R, S],
        ta: ClassTag[A],
        tb: ClassTag[B],
        tc: ClassTag[C],
        tz: ClassTag[Z],
        pa: Plain[A],
        pb: Plain[B],
        pc: Plain[C],
        pz: Plain[Z],
        cp: ContainerPair[T, U]
    ) =
      cp.applyTransformerPlain(as, bs, cs, f)

  extension [T[_]: Mappable, Z, A, B, C, R[_, _, _, _], S[_, _, _, _]](f: (A, B, C) => Mixed[Z, T])
    def apply(as: => Mixed[A, T], bs: => Mixed[B, T], cs: => Mixed[C, T])(using
        aug: AugmentC[R, S],
        ta: ClassTag[A],
        tb: ClassTag[B],
        tc: ClassTag[C],
        tz: ClassTag[Z],
        pa: Plain[A],
        pb: Plain[B],
        pc: Plain[C],
        pz: Plain[Z]
    ) =
      sequence(as, _ => bs, (_, _) => cs, f)

  extension [Z, A, B, C, D, R[_, _, _, _, _], S[_, _, _, _, _]](f: (A, B, C, D) => Z)
    def apply(as: Seq[A], bs: Seq[B], cs: Seq[C], ds: Seq[D])(using
        AugmentD[R, S],
        ClassTag[A],
        ClassTag[B],
        ClassTag[C],
        ClassTag[D],
        ClassTag[Z],
        Plain[A],
        Plain[B],
        Plain[C],
        Plain[D],
        Plain[Z]
    ) =
      augment(f)(as, bs, cs, ds)

  extension [T[_]: Mappable, Z, A, B, C, D, R[_, _, _, _, _], S[_, _, _, _, _]](f: (A, B, C, D) => Mixed[Z, T])
    def apply(as: => Mixed[A, T], bs: => Mixed[B, T], cs: => Mixed[C, T], ds: => Mixed[D, T])(using
        aug: AugmentD[R, S],
        ta: ClassTag[A],
        tb: ClassTag[B],
        tc: ClassTag[C],
        td: ClassTag[D],
        tz: ClassTag[Z],
        pa: Plain[A],
        pb: Plain[B],
        pc: Plain[C],
        pd: Plain[D],
        pz: Plain[Z]
    ) =
      sequence(as, _ => bs, (_, _) => cs, (_, _, _) => ds, f)

  extension [A](as: Seq[A])
    def headE =
      if (as.isEmpty)
        Left(NoSuchElementException("head of empty sequence"))
      else
        Right(as(0))

    def applyIndex(i: Int) =
      as.lift(i).get

    def apply[T[_]: Mappable](t: T[Int]) = -1

    def liftE(i: Int) =
      if (i < 0 || i >= as.length)
        Left(IndexOutOfBoundsException(f"Out of bounds for sequence of length ${as.length}: ${i}"))
      else
        Right(as.apply(i))

    def indexOfE(a: A) =
      val i = as.indexOf(a)
      if i == -1 then Left(IndexOutOfBoundsException(f"Value not found in list: ${a}"))
      else Right(i)

    def findE(pred: A => Boolean) =
      as.find(pred) match
        case None =>
          Left(NoSuchElementException(f"No such element found in list"))
        case Some(x) =>
          Right(x)

  extension [A](as: Array[A]) def head = as.toList.head

  def checked[E, Z](res: => Z, b: Boolean, err: E): Either[E, Z] =
    if b then Left(err)
    else Right(res)

  extension [U[_], T[_, _], V[_, _], A](
      as: U[Seq[A]]
  )(using eith: EitherishOne[U, T], e: Eitherish[T], e1: EitherishPair[T, V], ta: ClassTag[A])

    @targetName("headUS")
    def head: V[NoSuchElementException, A] =
      as.convertToEitherish().head

    def drop(n: Int): V[Nothing, Seq[A]] =
      as.convertToEitherish().drop(n)

  extension [T[_, _], U[_, _], E, A](
      as: T[E, Seq[A]]
  )(using eith: Eitherish[T], e1: EitherishPair[T, U], ta: ClassTag[A])

    def head: U[E | NoSuchElementException, A] =

      val res = sequence(
        as.asEitherP(),
        s =>
          val res =
            if s.isEmpty then NoSuchElementException("Head of empty sequence").toLeftP()
            else s.head.toRightP()
          res.asEither()
      )
      res.fromEitherP()

    def drop(n: Int): U[E, Seq[A]] =

      val res = sequence(
        as.asEitherP(),
        _.drop(n).toRightP().asEither()
      )
      res.fromEitherP()

    def apply(i: Int) =

      val res = sequence(
        as.asEitherP(),
        s =>
          val res =
            if i < 0 || i > s.length then
              IndexOutOfBoundsException(f"Out of bounds for sequence of length ${s.length}: ${i}").toLeftP()
            else s(i).toRightP()
          res.asEither()
      )
      res.fromEitherP()

  extension [A, T[_]: Mappable](as: => T[Seq[A]])

    def lastElement = image(as, s => checked(s.last, s.isEmpty, NoSuchElementException()))
    def applyZ[U[_]: Mappable](u: U[Int]) = u.map(i => as.map(_.apply(i)))

  extension [U[_], T[_, _], V[_, _], A](
      as: U[Array[A]]
  )(using eith: EitherishOne[U, T], e: Eitherish[T], e1: EitherishPair[T, V], ta: ClassTag[A])

    def head: V[NoSuchElementException, A] =
      as.convertToEitherish().head

    def apply(i: Int) =
      as.convertToEitherish().applyZ(i)

  extension [T[_, _], E, A, U[_, _]](
      as: T[E, Array[A]]
  )(using eith: Eitherish[T], e1: EitherishPair[T, U], ta: ClassTag[A])

    @targetName("headEA")
    def head: U[E | NoSuchElementException, A] =

      val res = sequence(
        as.asEitherP(),
        s =>
          val res =
            if s.isEmpty then NoSuchElementException("Head of empty array").toLeftP()
            else s.head.toRightP()
          res.asEither()
      )
      res.fromEitherP()

    def applyZ(i: Int): U[E | IndexOutOfBoundsException, A] = apply(i)

    @targetName("applyEA")
    def apply(i: Int) =

      val res = sequence(
        as.asEitherP(),
        s =>
          val res =
            if i < 0 || i >= s.length then
              IndexOutOfBoundsException(f"Out of bounds for sequence of length ${s.length}: ${i}").toLeftP()
            else s(i).toRightP()
          res.asEither()
      )
      res.fromEitherP()

  extension [A, T[_]: Mappable](as: => T[Array[A]])

    def drop(n: Int) = image(as, _.drop(n))
    def take(n: Int) = image(as, _.take(n))

  /** Numeric */

  extension [A](as: Seq[A])(using Numeric[A], ClassTag[A])

    def +(as1: Seq[A]) = augment(Numeric[A].plus)(as, as1)
    def *(as1: Seq[A]) = augment(Numeric[A].times)(as, as1)
    def -(as1: Seq[A]) = augment(Numeric[A].minus)(as, as1)
    def +(as1: A => Seq[A]) = augment(Numeric[A].plus)(as, as1)
    def *(as1: A => Seq[A]) = augment(Numeric[A].times)(as, as1)
    def -(as1: A => Seq[A]) = augment(Numeric[A].minus)(as, as1)

  extension [A](a: A)(using Numeric[A], ClassTag[A], Plain[A])

    def +[T[_]: Mappable](as: T[A]) = augment(Numeric[A].plus)(a, as)
    def *[T[_]: Mappable](as: T[A]) = augment(Numeric[A].times)(a, as)
    def -[T[_]: Mappable](as: T[A]) = augment(Numeric[A].minus)(a, as)

  extension [A, T[_]](as: T[A])(using Numeric[A], ClassTag[A], Mappable[T], Plain[A])

    def +(as1: A | T[A]) = augment(Numeric[A].plus)(as, as1)
    def -(as1: A | T[A]) = augment(Numeric[A].minus).apply(as, as1)

    def *(as1: A | T[A] | (() => T[A])) =
      as1 match
        case f: (() => _) => augment(Numeric[A].times).applyStandardForm(as, _ => f().asInstanceOf[T[A]])
        case _            => augment(Numeric[A].times)(as, as1.asInstanceOf[A | T[A]])

  extension [A](as: Seq[A])(using Fractional[A], ClassTag[A], Plain[A])
    def /(as1: Seq[A]) = augment(Fractional[A].div)(as, as1)
    def /(as1: A => Seq[A]) = augment(Fractional[A].div)(as, as1)

  extension [A](a: A)(using Fractional[A], ClassTag[A], Plain[A])
    def /[T[_]: Mappable](as: T[A]) = augment(Fractional[A].div)(a, as)
    def divFrac[T[_]: Mappable](as: T[A]) = augment(Fractional[A].div)(a, as)

  extension [A, T[_]](as: T[A])(using Fractional[A], ClassTag[A], Mappable[T], Plain[A])
    def /(as1: A | T[A]) = augment(Fractional[A].div)(as, as1)

  extension [E1, A](as: Either[E1, A])(using Numeric[A], ClassTag[A], Plain[A])
    def +[E2](as1: A | Either[E2, A]) =
      sequence(as, as1, Numeric[A].plus)

  extension (n: Int)
    def /[U[_], T[_, _], V[_, _]](
        ps: U[Int]
    )(using eith: EitherishOne[U, T], e: Eitherish[T], e1: EitherishPair[T, V]) =
      n.divide(ps.convertToEitherish())

  extension (n: Int)

    def divide[T[_, _]: Eitherish, U[_, _], E](ps: T[E, Int])(using e1: EitherishPair[T, U]) =
      val res = sequence(
        n,
        ps.asEitherP(),
        (a, b) =>
          val res =
            if b == 0 then ArithmeticException("division by zero").toLeftP()
            else (a / b).toRightP()
          res.asEither()
      )
      res.fromEitherP()

    def /[T[_, _]: Eitherish, U[_, _], E](ps: T[E, Int])(using e1: EitherishPair[T, U]) =

      val res = sequence(
        n,
        ps.asEitherP(),
        (a, b) =>
          val res =
            if b == 0 then ArithmeticException("division by zero").toLeftP()
            else (a / b).toRightP()
          res.asEither()
      )
      res.fromEitherP()

  /** Strings */

  extension [U[_], T[_, _], V[_, _]](
      as: U[String]
  )(using eith: EitherishOne[U, T], e: Eitherish[T], e1: EitherishPair[T, V])

    def head: V[NoSuchElementException, Char] =
      as.convertToEitherish().head

    def toInt: V[NumberFormatException, Int] =
      as.convertToEitherish().toInt

  extension [T[_, _], E, U[_, _]](as: T[E, String])(using eith: Eitherish[T], e1: EitherishPair[T, U])

    def first: U[E | NoSuchElementException, Char] = head

    def head: U[E | NoSuchElementException, Char] =

      val res = sequence(
        as.asEitherP(),
        s =>
          val res =
            if s.isEmpty then NoSuchElementException("Head of empty string").toLeftP()
            else s.head.toRightP()
          res.asEither()
      )
      res.fromEitherP()

    def toInt =

      val res = sequence(
        as.asEitherP(),
        s =>
          val res =
            if !s.toIntOption.isDefined then NumberFormatException(f"String not in numerical format: [$s]").toLeftP()
            else s.toInt.toRightP()
          res.asEither()
      )
      res.fromEitherP()

  extension [T[_]](as: T[String])(using Mappable[T], Plain[String], Plain[Int])
    def +(as1: String | T[String]) = augment((s1: String, s2: String) => s1 + s2)(as, as1)
    def *(ns: Int | T[Int]) = augment((s: String, n: Int) => s)(as, ns)

    def trim() = image(as, _.trim)
    def slice(from: Int, until: Int) =
      image(as, from.unit(), until.unit(), _.slice(_, _))

    def take(n: Int) = image(as, _.take(n))
    def drop(n: Int) = image(as, _.drop(n))
    def split(sep: String = " ") = image(as, _.split(sep))
    def splitA(sep: String = " ") = image(as, _.split(sep).toList)
    def replaceAll(s1: String, s2: String) =
      val f2Opt = image(as, s => s.replaceAll(_, _))
      f2Opt.map(_(s1, s2))

    def toUpperCase() = image(as, _.toUpperCase)
    def toLowerCase() = image(as, _.toLowerCase)

  /** Extensions that can be used for null coalescing */

  extension [A](a: => A)

    def toOption =
      try
        Option(a)
      catch case _ => None

    def toEither =
      try
        Right(a)
      catch
        case e =>
          Left(e)

    def toEitherR =
      val res: Either[Nothing, A] =
        Right(a)
      res

    def toTry =
      Try(a)
