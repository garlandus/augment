package typeconstpm

import scala.jdk.CollectionConverters.*
import com.google.common.collect._

case class NamedPair[A, B](name: String, a: A, b: B)
type Id[A] = A
type GuavaTB[A, B, C] = Table[A, B, C]
type GuavaStrMultimap = [A] =>> ListMultimap[String, A]

/** Implementations of explicit type constructor polymorphism */

given TypeConstructorPmA[GuavaStrMultimap, GuavaStrMultimap] with
  def tag: String = "Guava list multimap"

  def applyA[Z, A](f: A => Z, derived: GuavaStrMultimap[A]) =
    val keys = derived.keySet().asScala.toList
    val multimap1: ListMultimap[String, Z] = MultimapBuilder.treeKeys().arrayListValues().build()
    keys.foreach(k => derived.get(k).asScala.map(f).map(multimap1.get(k).add(_)))
    multimap1

given TypeConstructorPmB[NamedPair, Id] with
  def tag: String = "Named pair"

  def applyB[Z, A, B](f: (A, B) => Z, derived: NamedPair[A, B]): Z =
    f(derived.a, derived.b)

given TypeConstructorPmC[GuavaTB, List] with
  def tag: String = "Guava table"

  def applyC[Z, A, B, C](f: (A, B, C) => Z, derived: Table[A, B, C]) =
    val cells = derived.cellSet().asScala.toList
    cells.map(c => f(c.getRowKey(), c.getColumnKey(), c.getValue()))

given TypeConstructorPmC[TreeBasedTable, List] with
  def tag: String = "Guava tree-based table"

  def applyC[Z, A, B, C](f: (A, B, C) => Z, derived: TreeBasedTable[A, B, C]) =
    derived.cellSet().asScala.toList.map(c => f(c.getRowKey(), c.getColumnKey(), c.getValue()))
