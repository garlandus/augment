package typeconstpm

import augmented._
import scala.reflect.ClassTag

/** Explicit type constructor polymorphism */

trait TypeConstructorPmA[T[_], U[_]]:
  def tag: String
  def applyA[Z, A](f: A => Z, derived: T[A]): U[Z]

trait TypeConstructorPmB[T[_, _], U[_]]:
  def tag: String
  def applyB[Z, A, B](f: (A, B) => Z, derived: T[A, B]): U[Z]

trait TypeConstructorPmC[T[_, _, _], U[_]]:
  def tag: String
  def applyC[Z, A, B, C](f: (A, B, C) => Z, derived: T[A, B, C]): U[Z]

object Extensions:

  extension [Z, A, R[_, _], S[_, _]](f: A => Z)
    def apply[T[_], U[_]](
        derived: T[A]
    )(using ca: AugmentA[R, S], cz: ClassTag[Z], tcp: TypeConstructorPmA[T, U]): U[Z] =
      tcp.applyA(f, derived)

  extension [Z, A, B, R[_, _, _], S[_, _, _]](f: (A, B) => Z)
    def apply[T[_, _], U[_]](
        derived: T[A, B]
    )(using ca: AugmentB[R, S], cz: ClassTag[Z], tcp: TypeConstructorPmB[T, U]): U[Z] =
      tcp.applyB(f, derived)

  extension [Z, A, B, C, R[_, _, _, _], S[_, _, _, _]](f: (A, B, C) => Z)
    def apply[T[_, _, _], U[_]](derived: T[A, B, C])(using cz: ClassTag[Z], tcp: TypeConstructorPmC[T, U]): U[Z] =
      tcp.applyC(f, derived)
