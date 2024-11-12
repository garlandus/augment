import comprehension._
import shape._

import com.google.common.collect._

/** Third party implementations: Guava, etc. */

/** Guava comprehensions */

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
