package mappable

import augmented.augment
import augmented.given
import shape.{DepTB, flat, Mixed}

import scala.reflect.ClassTag
import scala.util._
import scala.annotation.targetName

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

  def product[A, B](as: T[A], bs: T[B]): T[(A, B)]

  def mproduct[A, B](as: T[A], bs: DepTB[T, A, B]): T[(A, B)] =
    as.flatMap: a =>
      bs(a).map:
        (a, _)

  /** The symbol ⨉ means either:
    * - standard Cartesian product (applicative case), or
    * - extended Cartesian product, i.e. dependent sum Σ (monadic case)
    */
  extension [T[_], A](as: T[A])
    infix def ⨉[B, Factor](bs: Factor)(using p: Productive[T, A, B, Factor]): T[(A, B)] =
      p.product(as, bs)

  /** not currently used, but provided for reference */
  def ap[A, B](fs: T[A => B])(as: T[A]): T[B] =
    map(product(fs, as)):
      case (f, a) => f(a)

  extension [A](as: T[A]) def retry(n: Int): T[A] = ???
  def isDelayed: Boolean = true

  extension [A](as: => Mixed[A, T])(using ct: ClassTag[A])
    def toDerivedFromMixed(): T[A] =
      (() => as).unitFromThunk()

  def sleepMillis(ms: Int): T[Unit] =
    Thread.sleep(ms.toLong).unit()

  def afterMillis[A](ms: Int)(a: => A): T[A] =
    sleepMillis(ms).flatMap(_ => a.unit())

@FunctionalInterface
trait Productive[T[_], A, B, Factor]:
  def product(xs: T[A], ys: Factor): T[(A, B)]

extension [A, B, T[_]: Mappable](xs: T[(A, B)])
  infix def ⨉[Z, Factor](ys: Factor)(using p: Productive[T, (A, B), Z, Factor]): T[(A, B, Z)] =
    p.product(xs, ys).flat

extension [A, B, C, T[_]: Mappable](xs: T[(A, B, C)])
  @targetName("productD")
  infix def ⨉[Z, Factor](ys: Factor)(using p: Productive[T, (A, B, C), Z, Factor]): T[(A, B, C, Z)] =
    p.product(xs, ys).flat

extension [A, B, C, D, T[_]: Mappable](xs: T[(A, B, C, D)])
  @targetName("productE")
  infix def ⨉[Z, Factor](ys: Factor)(using p: Productive[T, (A, B, C, D), Z, Factor]): T[(A, B, C, D, Z)] =
    p.product(xs, ys).flat

extension [A, B, C, D, E, T[_]: Mappable](xs: T[(A, B, C, D, E)])
  @targetName("productF")
  infix def ⨉[Z, Factor](ys: Factor)(using p: Productive[T, (A, B, C, D, E), Z, Factor]): T[(A, B, C, D, E, Z)] =
    p.product(xs, ys).flat

extension [A, B, C, D, E, F, T[_]: Mappable](xs: T[(A, B, C, D, E, F)])
  @targetName("productG")
  infix def ⨉[Z, Factor](ys: Factor)(using p: Productive[T, (A, B, C, D, E, F), Z, Factor]): T[(A, B, C, D, E, F, Z)] =
    p.product(xs, ys).flat

type EitherX = [E] =>> [A] =>> Either[E, A]
type RightX = [E] =>> [A] =>> Right[E, A]
type OptionE[E, A] = Option[A]
type TryE[E, A] = Try[A]

extension [A](as: Seq[A])
  @targetName("ext_seqC")
  infix def ⨉[B, Factor](bs: Factor)(using p: Productive[Seq, A, B, Factor]): Seq[(A, B)] =
    p.product(as, bs)
