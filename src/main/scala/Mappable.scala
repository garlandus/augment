package mappable

import augmented.augment
import augmented.given
import shape.Mixed

import scala.reflect.ClassTag
import scala.util._

trait Mappable[T[_]]:

  extension [A](as: T[A]) def hasValue(): Boolean
  extension [A](as: T[A]) def value(): A

  extension [A](a: => A) def unit(): T[A]
  extension [A](as: T[A]) def map[B](f: A => B): T[B]
  extension [A](as: T[A]) def flatMap[B](f: A => T[B]): T[B]

  extension [A](as: () => Mixed[A, T])
    def unitFromThunk()(using ClassTag[A]): T[A] =
      (as() match
        case a: A =>
          a
        case _ =>
          as().asInstanceOf[T[A]].value()
      ).unit()

  def product[A, B](as: T[A], bs: T[B]): T[(A, B)] = ???
  def product[A, B, C](as: T[A], bs: T[B], cs: T[C]): T[(A, B, C)] =
    map(product(as, product(bs, cs))):
      case (a, (b, c)) => (a, b, c)
  def product[A, B, C, D](as: T[A], bs: T[B], cs: T[C], ds: T[D]): T[(A, B, C, D)] =
    map(product(as, product(bs, product(cs, ds)))):
      case (a, (b, (c, d))) => (a, b, c, d)
  def product[A, B, C, D, E](as: T[A], bs: T[B], cs: T[C], ds: T[D], es: T[E]): T[(A, B, C, D, E)] =
    map(product(as, product(bs, product(cs, product(ds, es))))):
      case (a, (b, (c, (d, e)))) => (a, b, c, d, e)
  def product[A, B, C, D, E, F](as: T[A], bs: T[B], cs: T[C], ds: T[D], es: T[E], fs: T[F]): T[(A, B, C, D, E, F)] =
    map(product(as, product(bs, product(cs, product(ds, product(es, fs)))))):
      case (a, (b, (c, (d, (e, f))))) => (a, b, c, d, e, f)
  def product[A, B, C, D, E, F, G](
      as: T[A],
      bs: T[B],
      cs: T[C],
      ds: T[D],
      es: T[E],
      fs: T[F],
      gs: T[G]
  ): T[(A, B, C, D, E, F, G)] =
    map(product(as, product(bs, product(cs, product(ds, product(es, product(fs, gs))))))):
      case (a, (b, (c, (d, (e, (f, g)))))) => (a, b, c, d, e, f, g)

  /** not currently used, but provided for reference */
  def ap[A, B](fs: T[A => B])(as: T[A]): T[B] =
    map(product(fs, as)):
      case (f, a) => f(a)

  extension [A](as: T[A]) def retry(n: Int): T[A] = ???
  def isDelayed: Boolean = true

  extension [A](as: => Mixed[A, T])(using ct: ClassTag[A])
    def toDerivedFromMixed(): T[A] =
      (() => as).unitFromThunk()

trait Applicative[T[_]] extends Mappable[T]:
  def tag: String = "Applicative"

/** see https://scalawithcats.com/dist/scala-with-cats.html */
def traversal[T[_], A, B](values: Seq[A])(f: A => T[B])(using ap: Applicative[T]): T[List[B]] =
  values.foldLeft(List.empty[B].unit()): (acc, a) =>
    val item = f(a)
    for
      acc <- acc
      item <- item
    yield acc :+ item

/** used from Java */
trait MappableT[X]:
  def hasValue(): Boolean
  def value(): X
  def retry(n: Int): MappableT[X]

  def getOrig(): MappableW[X, ?]
  def mappable() = getOrig().getMappable()

  def getFnValueB1[Z, B](f: (X, B) => Z, b: B): MappableT[Z]
  def getFnValueB2[Z, A](f: (A, X) => Z, a: A): MappableT[Z]

  def getFnValueC1[Z, B, C](f: (X, B, C) => Z, b: B, c: C): MappableT[Z]
  def getFnValueC2[Z, A, C](f: (A, X, C) => Z, a: A, c: C): MappableT[Z]
  def getFnValueC3[Z, A, B](f: (A, B, X) => Z, a: A, b: B): MappableT[Z]

  def getFnValueD1[Z, B, C, D](f: (X, B, C, D) => Z, b: B, c: C, d: D): MappableT[Z]
  def getFnValueD2[Z, A, C, D](f: (A, X, C, D) => Z, a: A, c: C, d: D): MappableT[Z]
  def getFnValueD3[Z, A, B, D](f: (A, B, X, D) => Z, a: A, b: B, d: D): MappableT[Z]
  def getFnValueD4[Z, A, B, C](f: (A, B, C, X) => Z, a: A, b: B, c: C): MappableT[Z]

  def getFnValueE1[Z, B, C, D, E](f: (X, B, C, D, E) => Z, b: B, c: C, d: D, e: E): MappableT[Z]
  def getFnValueE2[Z, A, C, D, E](f: (A, X, C, D, E) => Z, a: A, c: C, d: D, e: E): MappableT[Z]
  def getFnValueE3[Z, A, B, D, E](f: (A, B, X, D, E) => Z, a: A, b: B, d: D, e: E): MappableT[Z]
  def getFnValueE4[Z, A, B, C, E](f: (A, B, C, X, E) => Z, a: A, b: B, c: C, e: E): MappableT[Z]
  def getFnValueE5[Z, A, B, C, D](f: (A, B, C, D, X) => Z, a: A, b: B, c: C, d: D): MappableT[Z]

case class MappableW[X, T[_]: Mappable](x: T[X]) extends MappableT[X]:

  def getMappable(): T[X] = x
  def getOrig() = this
  def hasValue() = x.hasValue()
  def value() = x.value()
  def retry(n: Int) = MappableW(x.retry(n))

  def getFnValueB1[Z, B](f: (X, B) => Z, b: B): MappableT[Z] =
    MappableW(augment(f) applyRectangular (x, b.unit()))
  def getFnValueB2[Z, A](f: (A, X) => Z, a: A): MappableT[Z] =
    MappableW(augment(f) applyRectangular (a.unit(), x))

  def getFnValueC1[Z, B, C](f: (X, B, C) => Z, b: B, c: C): MappableT[Z] =
    MappableW(augment(f) applyRectangular (x, b.unit(), c.unit()))
  def getFnValueC2[Z, A, C](f: (A, X, C) => Z, a: A, c: C): MappableT[Z] =
    MappableW(augment(f) applyRectangular (a.unit(), x, c.unit()))
  def getFnValueC3[Z, A, B](f: (A, B, X) => Z, a: A, b: B): MappableT[Z] =
    MappableW(augment(f) applyRectangular (a.unit(), b.unit(), x))

  def getFnValueD1[Z, B, C, D](f: (X, B, C, D) => Z, b: B, c: C, d: D): MappableT[Z] =
    MappableW(augment(f) applyRectangular (x, b.unit(), c.unit(), d.unit()))
  def getFnValueD2[Z, A, C, D](f: (A, X, C, D) => Z, a: A, c: C, d: D): MappableT[Z] =
    MappableW(augment(f) applyRectangular (a.unit(), x, c.unit(), d.unit()))
  def getFnValueD3[Z, A, B, D](f: (A, B, X, D) => Z, a: A, b: B, d: D): MappableT[Z] =
    MappableW(augment(f) applyRectangular (a.unit(), b.unit(), x, d.unit()))
  def getFnValueD4[Z, A, B, C](f: (A, B, C, X) => Z, a: A, b: B, c: C): MappableT[Z] =
    MappableW(augment(f) applyRectangular (a.unit(), b.unit(), c.unit(), x))

  def getFnValueE1[Z, B, C, D, E](f: (X, B, C, D, E) => Z, b: B, c: C, d: D, e: E): MappableT[Z] =
    MappableW(augment(f) applyRectangular (x, b.unit(), c.unit(), d.unit(), e.unit()))
  def getFnValueE2[Z, A, C, D, E](f: (A, X, C, D, E) => Z, a: A, c: C, d: D, e: E): MappableT[Z] =
    MappableW(augment(f) applyRectangular (a.unit(), x, c.unit(), d.unit(), e.unit()))
  def getFnValueE3[Z, A, B, D, E](f: (A, B, X, D, E) => Z, a: A, b: B, d: D, e: E): MappableT[Z] =
    MappableW(augment(f) applyRectangular (a.unit(), b.unit(), x, d.unit(), e.unit()))
  def getFnValueE4[Z, A, B, C, E](f: (A, B, C, X, E) => Z, a: A, b: B, c: C, e: E): MappableT[Z] =
    MappableW(augment(f) applyRectangular (a.unit(), b.unit(), c.unit(), x, e.unit()))
  def getFnValueE5[Z, A, B, C, D](f: (A, B, C, D, X) => Z, a: A, b: B, c: C, d: D): MappableT[Z] =
    MappableW(augment(f) applyRectangular (a.unit(), b.unit(), c.unit(), d.unit(), x))

trait Plain[A]:
  def isPlain: Boolean = true

trait Atomic[A]:
  def tag: String

trait ContainerPair[F[_], G[_]]:
  def tag: String
  def areSame(): Boolean

  def applyTransformerPlain[Z, A](as: F[G[A]], f: A => Z): F[G[Z]] =
    throw Exception(f"No transformer supplied for: $tag\n")

  def applyTransformer[Z, A](as: F[G[A]], f: A => F[G[Z]]): F[G[Z]] =
    throw Exception(f"No transformer supplied for: $tag\n")

  def applyTransformerPlain[Z, A, B](as: F[G[A]], bs: F[G[B]], f: (A, B) => Z): F[G[Z]] =
    throw Exception(f"No transformer supplied for: $tag\n")

  def applyTransformer[Z, A, B](as: F[G[A]], bs: F[G[B]], f: (A, B) => F[G[Z]]): F[G[Z]] =
    throw Exception(f"No transformer supplied for: $tag\n")

  def applyTransformerPlain[Z, A, B, C](as: F[G[A]], bs: F[G[B]], cs: F[G[C]], f: (A, B, C) => Z): F[G[Z]] =
    throw Exception(f"No transformer supplied for: $tag\n")

  def applyTransformer[Z, A, B, C](as: F[G[A]], bs: F[G[B]], cs: F[G[C]], f: (A, B, C) => F[G[Z]]): F[G[Z]] =
    throw Exception(f"No transformer supplied for: $tag\n")

trait ContainerTriple[F[_], G[_], H[_]]:
  def tag: String

type EitherX        = [E] =>> [A] =>> Either[E, A]
type RightX         = [E] =>> [A] =>> Right[E, A]
type OptionE[E, A]  = Option[A]
type TryE[E, A]     = Try[A]

/** Container types that behave similarly to Either
  */
trait Eitherish[T[_, _]]:
  val tag: String
  extension [E, A](e: E) def toLeft(): T[E, A]
  extension [E, A](a: A) def toRight(): T[E, A]
  extension [E, A](x: T[E, A]) def asEither()(using ClassTag[A]): Either[E, A]
  extension [E, A](x: Either[E, A]) def fromEither(): T[E, A]
  extension [U[_, _], E, A](x: Either[E, A])(using m: EitherishPair[T, U]) def fromEither(): U[E, A] = ???

/** Container types that behave similarly to Either, but have only one type parameter
  */
trait EitherishOne[U[_], T[_, _]: Eitherish]:
  val tag: String
  extension [A](as: U[A]) def convertToEitherish(): T[Nothing, A]

trait EitherishPair[T[_, _]: Eitherish, U[_, _]: Eitherish]:
  val tag: String
  extension [E, A](e: E) def toLeftP(): Either[E, A] = Left(e)
  extension [E, A](a: A) def toRightP(): Either[E, A] = Right(a)
  extension [E, A](x: T[E, A]) def asEitherP()(using ClassTag[A]): Either[E, A]
  extension [E, A](x: Either[E, A]) def fromEitherP(): U[E, A]

object MappableExtensions:

  extension [T[_]: Mappable, U[_]: Mappable, A](x: T[U[A]]) infix def finalValue2() = x.value().value()
  extension [T[_]: Mappable, U[_]: Mappable, A](x: T[U[A]])
    infix def hasFinalValue2() =
      x.hasValue() && x.value().hasValue()

  extension [T[_]: Mappable, U[_]: Mappable, V[_]: Mappable, A](x: T[U[V[A]]])
    infix def finalValue() = x.value().value().value()
  extension [T[_]: Mappable, U[_]: Mappable, V[_]: Mappable, A](x: T[U[V[A]]])
    infix def hasFinalValue() = x.hasValue() && x.value().hasValue() && x.value().value().hasValue()
