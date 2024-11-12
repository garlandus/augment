package shape

import comprehension._
import mappable._
import mappable.given
import multiarray._
import util._

import scala.reflect.ClassTag
import scala.deriving.Mirror

/** Dependent sets */

type DepSetA[A]                   = () => Set[A]
type DepSetB[A, B]                = A => Set[B]
type DepSetC[A, B, C]             = (A, B) => Set[C]
type DepSetD[A, B, C, D]          = (A, B, C) => Set[D]
type DepSetE[A, B, C, D, E]       = (A, B, C, D) => Set[E]
type DepSetF[A, B, C, D, E, F]    = (A, B, C, D, E) => Set[F]
type DepSetG[A, B, C, D, E, F, G] = (A, B, C, D, E, F) => Set[G]

/** Generalized sets */

type GenSetA[A]                   = Set[A] | DepSetA[A]
type GenSetB[A, B]                = Set[B] | DepSetB[A, B]
type GenSetC[A, B, C]             = Set[C] | DepSetC[A, B, C]
type GenSetD[A, B, C, D]          = Set[D] | DepSetD[A, B, C, D]
type GenSetE[A, B, C, D, E]       = Set[E] | DepSetE[A, B, C, D, E]
type GenSetF[A, B, C, D, E, F]    = Set[F] | DepSetF[A, B, C, D, E, F]
type GenSetG[A, B, C, D, E, F, G] = Set[G] | DepSetG[A, B, C, D, E, F, G]

/** Dependent sequences */

type DepSeqA[A]                   = () => Seq[A]
type DepSeqB[A, B]                = A => Seq[B]
type DepSeqC[A, B, C]             = (A, B) => Seq[C]
type DepSeqD[A, B, C, D]          = (A, B, C) => Seq[D]
type DepSeqE[A, B, C, D, E]       = (A, B, C, D) => Seq[E]
type DepSeqF[A, B, C, D, E, F]    = (A, B, C, D, E) => Seq[F]
type DepSeqG[A, B, C, D, E, F, G] = (A, B, C, D, E, F) => Seq[G]

/** Generalized sequences */

type GenSeqA[A]                   = Seq[A] | DepSeqA[A]
type GenSeqB[A, B]                = Seq[B] | DepSeqB[A, B]
type GenSeqC[A, B, C]             = Seq[C] | DepSeqC[A, B, C]
type GenSeqD[A, B, C, D]          = Seq[D] | DepSeqD[A, B, C, D]
type GenSeqE[A, B, C, D, E]       = Seq[E] | DepSeqE[A, B, C, D, E]
type GenSeqF[A, B, C, D, E, F]    = Seq[F] | DepSeqF[A, B, C, D, E, F]
type GenSeqG[A, B, C, D, E, F, G] = Seq[G] | DepSeqG[A, B, C, D, E, F, G]

/** Dependent values */

type DepA[A]                      = () => A
type DepB[A, B]                   = A => B
type DepC[A, B, C]                = (A, B) => C
type DepD[A, B, C, D]             = (A, B, C) => D
type DepE[A, B, C, D, E]          = (A, B, C, D) => E
type DepF[A, B, C, D, E, F]       = (A, B, C, D, E) => F
type DepG[A, B, C, D, E, F, G]    = (A, B, C, D, E, F) => G

/** Derived types */

type Mixed[A, T[_]]               = T[A] | A
type MixedB[T[_], A, B]           = A => T[B] | B
type MixedC[T[_], A, B, C]        = (A, B) => T[C] | C
type MixedD[T[_], A, B, C, D]     = (A, B, C) => T[D] | D
type MixedE[T[_], A, B, C, D, E]  = (A, B, C, D) => T[E] | E

/** Dependent derived types */

type DepTA[T[_], A]                   = () => T[A]
type DepTB[T[_], A, B]                = A => T[B]
type DepTC[T[_], A, B, C]             = (A, B) => T[C]
type DepTD[T[_], A, B, C, D]          = (A, B, C) => T[D]
type DepTE[T[_], A, B, C, D, E]       = (A, B, C, D) => T[E]
type DepTF[T[_], A, B, C, D, E, F]    = (A, B, C, D, E) => T[F]
type DepTG[T[_], A, B, C, D, E, F, G] = (A, B, C, D, E, F) => T[G]

/** Generalized derived types */

type GenTA[T[_], A]                   = T[A] | DepTA[T, A]
type GenTB[T[_], A, B]                = T[B] | DepTB[T, A, B]
type GenTC[T[_], A, B, C]             = T[C] | DepTC[T, A, B, C]
type GenTD[T[_], A, B, C, D]          = T[D] | DepTD[T, A, B, C, D]
type GenTE[T[_], A, B, C, D, E]       = T[E] | DepTE[T, A, B, C, D, E]
type GenTF[T[_], A, B, C, D, E, F]    = T[F] | DepTF[T, A, B, C, D, E, F]
type GenTG[T[_], A, B, C, D, E, F, G] = T[G] | DepTG[T, A, B, C, D, E, F, G]

/** Some containers that can be returned by comprehensions */

type IdA[Z, A]              = Z
type SeqA[Z, A]             = Seq[Z]
type SeqNA[Z, A]            = Seq[Z]
type ArrayNA[Z, A]          = Array[Z]
type MapA[Z, A]             = Map[A, Z]
type SetA[Z, A]             = Set[Z]

opaque type SeqParallelA[Z, A]              = SeqA[Z, A]
opaque type SeqParallelB[Z, A, B]           = SeqB[Z, A, B]
opaque type SeqParallelC[Z, A, B, C]        = SeqC[Z, A, B, C]
opaque type SeqParallelD[Z, A, B, C, D]     = SeqD[Z, A, B, C, D]
opaque type SeqParallelE[Z, A, B, C, D, E]  = SeqE[Z, A, B, C, D, E]

opaque type SeqParallelNB[Z, A, B]          = SeqNB[Z, A, B]
opaque type SeqParallelNC[Z, A, B, C]       = SeqNC[Z, A, B, C]
opaque type SeqParallelND[Z, A, B, C, D]    = SeqND[Z, A, B, C, D]
opaque type SeqParallelNE[Z, A, B, C, D, E] = SeqNE[Z, A, B, C, D, E]

opaque type MapParallelA[Z, A]              = MapA[Z, A]
opaque type MapParallelB[Z, A, B]           = MapB[Z, A, B]
opaque type MapParallelC[Z, A, B, C]        = MapC[Z, A, B, C]
opaque type MapParallelD[Z, A, B, C, D]     = MapD[Z, A, B, C, D]
opaque type MapParallelE[Z, A, B, C, D, E]  = MapE[Z, A, B, C, D, E]

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

type IdE[Z, A, B, C, D, E]            = Z
type SeqE[Z, A, B, C, D, E]           = Seq[Z]
type SeqNE[Z, A, B, C, D, E]          = Seq[Seq[Seq[Seq[Seq[Z]]]]]
type ArrayNE[Z, A, B, C, D, E]        = Array[Array[Array[Array[Array[Z]]]]]
type MapE[Z, A, B, C, D, E]           = Map[A, Map[B, Map[C, Map[D, Map[E, Z]]]]]
type SetE[Z, A, B, C, D, E]           = Set[Z]

type IdF[Z, A, B, C, D, E, F]         = Z
type SeqF[Z, A, B, C, D, E, F]        = Seq[Z]
type SeqNF[Z, A, B, C, D, E, F]       = Seq[Seq[Seq[Seq[Seq[Seq[Z]]]]]]
type ArrayNF[Z, A, B, C, D, E, F]     = Array[Array[Array[Array[Array[Array[Z]]]]]]
type MapF[Z, A, B, C, D, E, F]        = Map[A, Map[B, Map[C, Map[D, Map[E, Map[F, Z]]]]]]
type SetF[Z, A, B, C, D, E, F]        = Set[Z]

type IdG[Z, A, B, C, D, E, F, G]      = Z
type SeqG[Z, A, B, C, D, E, F, G]     = Seq[Z]
type SeqNG[Z, A, B, C, D, E, F, G]    = Seq[Seq[Seq[Seq[Seq[Seq[Seq[Z]]]]]]]
type ArrayNG[Z, A, B, C, D, E, F, G]  = Array[Array[Array[Array[Array[Array[Array[Z]]]]]]]
type MapG[Z, A, B, C, D, E, F, G]     = Map[A, Map[B, Map[C, Map[D, Map[E, Map[F, Map[G, Z]]]]]]]
type SetG[Z, A, B, C, D, E, F, G]     = Set[Z]

type ProdB[X, A, B] =
  Mirror.Product:
    type MirroredType = X
    type MirroredMonoType = X
    type MirroredElemTypes = (A, B)
type ProdC[X, A, B, C] =
  Mirror.Product:
    type MirroredType = X
    type MirroredMonoType = X
    type MirroredElemTypes = (A, B, C)
type ProdD[X, A, B, C, D] =
  Mirror.Product:
    type MirroredType = X
    type MirroredMonoType = X
    type MirroredElemTypes = (A, B, C, D)
type ProdE[X, A, B, C, D, E] =
  Mirror.Product:
    type MirroredType = X
    type MirroredMonoType = X
    type MirroredElemTypes = (A, B, C, D, E)
type ProdF[X, A, B, C, D, E, F] =
  Mirror.Product:
    type MirroredType = X
    type MirroredMonoType = X
    type MirroredElemTypes = (A, B, C, D, E, F)
type ProdG[X, A, B, C, D, E, F, G] =
  Mirror.Product:
    type MirroredType = X
    type MirroredMonoType = X
    type MirroredElemTypes = (A, B, C, D, E, F, G)

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

def seq[A, B, C, D, E, F](genSeq: GenSeqF[A, B, C, D, E, F]): DepSeqF[A, B, C, D, E, F] =
  genSeq match
    case l: Seq[_] => ((a, b, c, d, e) => l.asInstanceOf[Seq[F]])
    case _         => genSeq.asInstanceOf[DepSeqF[A, B, C, D, E, F]]

def seq[A, B, C, D, E, F, G](genSeq: GenSeqG[A, B, C, D, E, F, G]): DepSeqG[A, B, C, D, E, F, G] =
  genSeq match
    case l: Seq[_] => ((a, b, c, d, e, f) => l.asInstanceOf[Seq[G]])
    case _         => genSeq.asInstanceOf[DepSeqG[A, B, C, D, E, F, G]]

type F0[A]                            = () => A
type F1[A, B]                         = A => B
type F2[A, B, C]                      = (A, B) => C
type F3[A, B, C, D]                   = (A, B, C) => D
type F4[A, B, C, D, E]                = (A, B, C, D) => E
type F5[A, B, C, D, E, F]             = (A, B, C, D, E) => F
type F6[A, B, C, D, E, F, G]          = (A, B, C, D, E, F) => G

type MixedFormsA[A]                   = F0[A] | A
type MixedFormsB[A, B]                = F1[A, B] | B
type MixedFormsC[A, B, C]             = F2[A, B, C] | F1[B, C] | C
type MixedFormsD[A, B, C, D]          = F3[A, B, C, D] | F2[B, C, D] | F1[C, D] | D
type MixedFormsE[A, B, C, D, E]       = F4[A, B, C, D, E] | F3[B, C, D, E] | F2[C, D, E] | F1[D, E] | E
type MixedFormsF[A, B, C, D, E, F]    = F5[A, B, C, D, E, F] | F4[B, C, D, E, F] | F3[C, D, E, F] | F2[D, E, F] |
                                        F1[E, F] | F
type MixedFormsG[A, B, C, D, E, F, G] = F6[A, B, C, D, E, F, G] | F5[B, C, D, E, F, G] | F4[C, D, E, F, G] |
                                        F3[D, E, F, G] | F2[E, F, G] | F1[F, G] | G

type MixedFormsTA[T[_], A]                    = MixedFormsA[Mixed[A, T]]
type MixedFormsTB[T[_], A, B]                 = MixedFormsB[A, Mixed[B, T]]
type MixedFormsTC[T[_], A, B, C]              = MixedFormsC[A, B, Mixed[C, T]]
type MixedFormsTD[T[_], A, B, C, D]           = MixedFormsD[A, B, C, Mixed[D, T]]
type MixedFormsTE[T[_], A, B, C, D, E]        = MixedFormsE[A, B, C, D, Mixed[E, T]]
type MixedFormsTF[T[_], A, B, C, D, E, F]     = MixedFormsF[A, B, C, D, E, Mixed[F, T]]
type MixedFormsTG[T[_], A, B, C, D, E, F, G]  = MixedFormsG[A, B, C, D, E, F, Mixed[G, T]]

type MixedThk[A, T[_]]                        = () => (T[A] | A)

def toStdFormA[A](f: MixedFormsA[A])(using ClassTag[A]) =
  f match
    case f0: F0[_]                => toStandardFormA(f0.asInstanceOf[F0[A]])
    case a: A                     => toStandardFormA(a)
  
def toStdFormB[A, B](f: MixedFormsB[A, B])(using ClassTag[A], ClassTag[B]): A => B =
  f match
    case f1: F1[_, _]             => toStandardFormB(f1.asInstanceOf[F1[A, B]])
    case b: B                     => toStandardFormB(b)
  
def toStdFormC[A, B, C](f: MixedFormsC[A, B, C])(using ClassTag[A], ClassTag[B], ClassTag[C]):(A, B) => C =
  f match
    case f2: F2[_, _, _]          => toStandardFormC(f2.asInstanceOf[F2[A, B, C]])
    case f1: F1[_, _]             => toStandardFormC(f1.asInstanceOf[F1[B, C]])
    case c: C                     => toStandardFormC(c)
  
def toStdFormD[A, B, C, D](f: MixedFormsD[A, B, C, D])(using ClassTag[A], ClassTag[B], ClassTag[C], ClassTag[D]):
    (A, B, C) => D =
  f match
    case f3: F3[_, _, _, _]       => toStandardFormD(f3.asInstanceOf[F3[A, B, C, D]])
    case f2: F2[_, _, _]          => toStandardFormD(f2.asInstanceOf[F2[B, C, D]])
    case f1: F1[_, _]             => toStandardFormD(f1.asInstanceOf[F1[C, D]])
    case d: D                     => toStandardFormD(d)

def toStdFormE[A, B, C, D, E](f: MixedFormsE[A, B, C, D, E])
    (using ClassTag[A], ClassTag[B], ClassTag[C], ClassTag[D], ClassTag[E]):(A, B, C, D) => E =
  f match
    case f4: F4[_, _, _, _, _]    => toStandardFormE(f4.asInstanceOf[F4[A, B, C, D, E]])
    case f3: F3[_, _, _, _]       => toStandardFormE(f3.asInstanceOf[F3[B, C, D, E]])
    case f2: F2[_, _, _]          => toStandardFormE(f2.asInstanceOf[F2[C, D, E]])
    case f1: F1[_, _]             => toStandardFormE(f1.asInstanceOf[F1[D, E]])
    case e: E                     => toStandardFormE(e)

def toStdFormF[A, B, C, D, E, F](f: MixedFormsF[A, B, C, D, E, F])
    (using ClassTag[A], ClassTag[B], ClassTag[C], ClassTag[D], ClassTag[E], ClassTag[F]):(A, B, C, D, E) => F =
  f match
    case f5: F5[_, _, _, _, _, _] => toStandardFormF(f5.asInstanceOf[F5[A, B, C, D, E, F]])
    case f4: F4[_, _, _, _, _]    => toStandardFormF(f4.asInstanceOf[F4[B, C, D, E, F]])
    case f3: F3[_, _, _, _]       => toStandardFormF(f3.asInstanceOf[F3[C, D, E, F]])
    case f2: F2[_, _, _]          => toStandardFormF(f2.asInstanceOf[F2[D, E, F]])
    case f1: F1[_, _]             => toStandardFormF(f1.asInstanceOf[F1[E, F]])
    case f0: F                    => toStandardFormF(f0)
  
def toStandardFormA[A](f: F0[A])                        = f()
def toStandardFormA[A](a: A)                            = a

def toStandardFormB[A, B](f: F1[A, B])                  = f
def toStandardFormB[A, B](b: B): F1[A, B]               = _ => b

def toStandardFormC[A, B, C](f: F2[A, B, C])            = f
def toStandardFormC[A, B, C](f: F1[B, C]): F2[A, B, C]  = (_, b) => f(b)
def toStandardFormC[A, B, C](c: C): F2[A, B, C]         = (_, _) => c

def toStandardFormD[A, B, C, D](f: F3[A, B, C, D])                                = f
def toStandardFormD[A, B, C, D](f: F2[B, C, D]): F3[A, B, C, D]                   = (_, b, c) => f(b, c)
def toStandardFormD[A, B, C, D](f: F1[C, D]): F3[A, B, C, D]                      = (_, _, c) => f(c)
def toStandardFormD[A, B, C, D](d: D): F3[A, B, C, D]                             = (_, _, _) => d

def toStandardFormE[A, B, C, D, E](f: F4[A, B, C, D, E]): F4[A, B, C, D, E]       = f
def toStandardFormE[A, B, C, D, E](f: F3[B, C, D, E]): F4[A, B, C, D, E]          = (_, b, c, d) => f(b, c, d)
def toStandardFormE[A, B, C, D, E](f: F2[C, D, E]): F4[A, B, C, D, E]             = (_, _, c, d) => f(c, d)
def toStandardFormE[A, B, C, D, E](f: F1[D, E]): F4[A, B, C, D, E]                = (_, _, _, d) => f(d)
def toStandardFormE[A, B, C, D, E](e: E): F4[A, B, C, D, E]                       = (_, _, _, _) => e

def toStandardFormF[A, B, C, D, E, F](f: F5[A, B, C, D, E, F])                    = f
def toStandardFormF[A, B, C, D, E, F](f: F4[B, C, D, E, F]): F5[A, B, C, D, E, F] = (_, b, c, d, e) => f(b, c, d, e)
def toStandardFormF[A, B, C, D, E, F](f: F3[C, D, E, F]): F5[A, B, C, D, E, F]    = (_, _, c, d, e) => f(c, d, e)
def toStandardFormF[A, B, C, D, E, F](f: F2[D, E, F]): F5[A, B, C, D, E, F]       = (_, _, _, d, e) => f(d, e)
def toStandardFormF[A, B, C, D, E, F](f: F1[E, F]): F5[A, B, C, D, E, F]          = (_, _, _, _, e) => f(e)
def toStandardFormF[A, B, C, D, E, F](f: F): F5[A, B, C, D, E, F]                 = (_, _, _, _, _) => f

def toDerived[T[_]: Mappable, A](as: => Mixed[A, T], delayed: Boolean)(using ct: ClassTag[A], pa: Plain[A]): T[A] =
  if !pa.isPlain then
    throw Exception("Type A is already a container (unexpected type inference)")
  if delayed then
    as.toDerivedFromMixed()
  else
    toDerived(as)

def toDerived[T[_]: Mappable, A](as: => Mixed[A, T])(using ct: ClassTag[A], pa: Plain[A]): T[A] =
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
  extension(b: Boolean)
    def option[A](a: A) =
      if b then Some(a) else None

object Extensions:
  extension[A, B, C](f: (A, B) => C)
    infix def andThen[D](g: C => D) =
      Function.untupled(f.tupled andThen g)

type Nested[T[_], U[_]] = [Z] =>> T[U[Z]]
type NestedV[T[_], U[_], V[_]] = [Z] =>> T[U[V[Z]]]
type NestedW[T[_], U[_], V[_], W[_]] = [Z] =>> T[U[V[W[Z]]]]
type NestedX[T[_], U[_], V[_], W[_], X[_]] = [Z] =>> T[U[V[W[X[Z]]]]]

