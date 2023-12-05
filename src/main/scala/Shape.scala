package shape

import comprehension._
import mappable._
import multiarray._
import util._

import scala.reflect.ClassTag
import scala.deriving.Mirror

/** Dependent sets */

type DepSetA[A]             = () => Set[A]
type DepSetB[A, B]          = A => Set[B]
type DepSetC[A, B, C]       = (A, B) => Set[C]
type DepSetD[A, B, C, D]    = (A, B, C) => Set[D]
type DepSetE[A, B, C, D, E] = (A, B, C, D) => Set[E]

/** Generalized sets */

type GenSetA[A]             = Set[A] | DepSetA[A]
type GenSetB[A, B]          = Set[B] | DepSetB[A, B]
type GenSetC[A, B, C]       = Set[C] | DepSetC[A, B, C]
type GenSetD[A, B, C, D]    = Set[D] | DepSetD[A, B, C, D]
type GenSetE[A, B, C, D, E] = Set[E] | DepSetE[A, B, C, D, E]

/** Dependent sequences */

type DepSeqA[A]             = () => Seq[A]
type DepSeqB[A, B]          = A => Seq[B]
type DepSeqC[A, B, C]       = (A, B) => Seq[C]
type DepSeqD[A, B, C, D]    = (A, B, C) => Seq[D]
type DepSeqE[A, B, C, D, E] = (A, B, C, D) => Seq[E]

/** Generalized sequences */

type GenSeqA[A]             = Seq[A] | DepSeqA[A]
type GenSeqB[A, B]          = Seq[B] | DepSeqB[A, B]
type GenSeqC[A, B, C]       = Seq[C] | DepSeqC[A, B, C]
type GenSeqD[A, B, C, D]    = Seq[D] | DepSeqD[A, B, C, D]
type GenSeqE[A, B, C, D, E] = Seq[E] | DepSeqE[A, B, C, D, E]

/** Derived types */

type Mixed[A, T[_]]         = A | T[A]

/** Dependent derived types */

type DepTA[T[_], A]             = () => T[A]
type DepTB[T[_], A, B]          = A => T[B]
type DepTC[T[_], A, B, C]       = (A, B) => T[C]
type DepTD[T[_], A, B, C, D]    = (A, B, C) => T[D]
type DepTE[T[_], A, B, C, D, E] = (A, B, C, D) => T[E]

/** Generalized derived types */

type GenTA[T[_], A]             = T[A] | DepTA[T, A]
type GenTB[T[_], A, B]          = T[B] | DepTB[T, A, B]
type GenTC[T[_], A, B, C]       = T[C] | DepTC[T, A, B, C]
type GenTD[T[_], A, B, C, D]    = T[D] | DepTD[T, A, B, C, D]
type GenTE[T[_], A, B, C, D, E] = T[E] | DepTE[T, A, B, C, D, E]

/** Some containers that can be returned by comprehensions */

type IdA[Z, A]              = Z
type SeqA[Z, A]             = Seq[Z]
type SeqNA[Z, A]            = Seq[Z]
type ArrayNA[Z, A]          = Array[Z]
type MapA[Z, A]             = Map[A, Z]
type SetA[Z, A]             = Set[Z]

type IdB[Z, A, B]           = Z
type SeqB[Z, A, B]          = Seq[Z]
type SeqNB[Z, A, B]         = Seq[Seq[Z]]
type ArrayNB[Z, A, B]       = Array[Array[Z]]
type MapB[Z, A, B]          = Map[A, Map[B, Z]]
type SetB[Z, A, B]          = Set[Z]

type IdC[Z, A, B, C]        = Z
type SeqC[Z, A, B, C]       = Seq[Z]
type SeqNC[Z, A, B, C]      = Seq[Seq[Seq[Z]]]
type ArrayNC[Z, A, B, C]    = Array[Array[Array[Z]]]
type MapC[Z, A, B, C]       = Map[A, Map[B, Map[C, Z]]]
type SetC[Z, A, B, C]       = Set[Z]

type IdD[Z, A, B, C, D]     = Z
type SeqD[Z, A, B, C, D]    = Seq[Z]
type SeqND[Z, A, B, C, D]   = Seq[Seq[Seq[Seq[Z]]]]
type ArrayND[Z, A, B, C, D] = Array[Array[Array[Array[Z]]]]
type MapD[Z, A, B, C, D]    = Map[A, Map[B, Map[C, Map[D, Z]]]]
type SetD[Z, A, B, C, D]    = Set[Z]

type IdE[Z, A, B, C, D, E]      = Z
type SeqE[Z, A, B, C, D, E]     = Seq[Z]
type SeqNE[Z, A, B, C, D, E]    = Seq[Seq[Seq[Seq[Seq[Z]]]]]
type ArrayNE[Z, A, B, C, D, E]  = Array[Array[Array[Array[Array[Z]]]]]
type MapE[Z, A, B, C, D, E]     = Map[A, Map[B, Map[C, Map[D, Map[E, Z]]]]]
type SetE[Z, A, B, C, D, E]     = Set[Z]

type ProdB[X, A, B] = Mirror.Product {
  type MirroredType = X; type MirroredMonoType = X; type MirroredElemTypes = (A, B)
}
type ProdC[X, A, B, C] = Mirror.Product {
  type MirroredType = X; type MirroredMonoType = X; type MirroredElemTypes = (A, B, C)
}
type ProdD[X, A, B, C, D] = Mirror.Product {
  type MirroredType = X; type MirroredMonoType = X; type MirroredElemTypes = (A, B, C, D)
}
type ProdE[X, A, B, C, D, E] = Mirror.Product {
  type MirroredType = X; type MirroredMonoType = X; type MirroredElemTypes = (A, B, C, D, E)
}
type ProdF[X, A, B, C, D, E, F] = Mirror.Product {
  type MirroredType = X; type MirroredMonoType = X; type MirroredElemTypes = (A, B, C, D, E, F)
}

/** Column vector, used for standard vectorization */

case class ColVector[A](vec: Vector[A])

object ColVector:
  def apply[A](a: A*): ColVector[A] =
    ColVector(a.toVector)

def seq[A, B](genSeq: GenSeqB[A, B]): DepSeqB[A, B] =
  genSeq match
    case l: Seq[_] => (a => l.asInstanceOf[Seq[B]])
    case _         => genSeq.asInstanceOf[DepSeqB[A, B]]

def seq[A, B, C](genSeq: GenSeqC[A, B, C]): DepSeqC[A, B, C] =
  genSeq match
    case l: Seq[_] => ((a, b) => l.asInstanceOf[Seq[C]])
    case _         => genSeq.asInstanceOf[DepSeqC[A, B, C]]

def seq[A, B, C, D](genSeq: GenSeqD[A, B, C, D]): DepSeqD[A, B, C, D] =
  genSeq match
    case l: Seq[_] => ((a, b, c) => l.asInstanceOf[Seq[D]])
    case _         => genSeq.asInstanceOf[DepSeqD[A, B, C, D]]

def seq[A, B, C, D, E](genSeq: GenSeqE[A, B, C, D, E]): DepSeqE[A, B, C, D, E] =
  genSeq match
    case l: Seq[_] => ((a, b, c, d) => l.asInstanceOf[Seq[E]])
    case _         => genSeq.asInstanceOf[DepSeqE[A, B, C, D, E]]

def toDerived[A, T[_]](as: Mixed[A, T])(using t: Mappable[T], ct: ClassTag[A]): T[A] =
  as match
    case a: A =>
      a.unit()
    case _ =>
      as.asInstanceOf[T[A]]

object SeqExtensions:

  extension [A](l: => Seq[Seq[A]])
    infix def until(b: Boolean, neutral: List[List[A]] = List(List[A]())) =
      if b then neutral else l

  extension [A](l: => Seq[A])
    infix def until(b: Boolean) =
      if b then List[A]() else l

    infix def where(fltr: A => Boolean) =
      l.filter(fltr)
    infix def where(b: Boolean) =
      if b then l else Seq[A]()

    infix def --(a: A) =
      (l diff List(a)).toList
    infix def --(l1: Seq[A]) =
      (l diff l1).toList

  extension [A](l: => Set[List[A]])
    infix def until(b: Boolean) =
      if b then Set(List[A]()) else l

  extension [A](l: => LazyList[A])
    infix def where(fltr: A => Boolean): LazyList[A] =
      l.filter(fltr)

object BooleanExtensions:
  extension (b: Boolean)
    def option[A](a: A) =
      if b then Some(a) else None

object FunctionExtensions:
  extension [A, B, C](f: (A, B) => C)
    infix def andThen[D](g: C => D) =
      Function.untupled(f.tupled andThen g)
