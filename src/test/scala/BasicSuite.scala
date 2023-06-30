import augmented._
import augmented.given
import basicdef._
import multiarray._
import shape._
import shape.SeqExtensions._
import util._
import util.StringExtensions._

import math.Ordering.Implicits.infixOrderingOps
import scala.collection.immutable.HashSet
import scala.util._

class BasicSuite extends munit.FunSuite:

  val mult = augment((a: Int, b: Int) => a * b)
  val add = augment((a: Int, b: Int, c: Int) => a + b + c)

  test("Pythagorean triangles"):
    val n = 30

    val res1 = select(1 to n, _ to n, _ to n, (a, b, c) => a * a + b * b == c * c)

    val res2 = select(1 to n, _ to n, _ to n, sq(_) + sq(_) == sq(_))

    val res3 =
      for
        a <- 1 to n
        b <- a to n
        c <- b to n
        if a * a + b * b == c * c
      yield (a, b, c)

    assertEquals(res1, res2)
    assertEquals(res1, res3)

  test("tetrahedron"):
    val n = 5

    val ct1 = count(1 to n, 1 to _, 1 to _)

    val ct2 =
      (for
        a <- 1 to n
        b <- 1 to a
        c <- 1 to b
      yield (a, b, c)).length

    assertEquals(ct1, ct2)
    assertEquals(ct1, 35)

  test("basic B"):
    val (as, bs) = ((3 to 5).toList, 7 to 9)

    mult crossCheck (as, bs,
    for
      a <- _
      b <- _
    yield a * b)

    mult crossCheck (as, bs,
    for
      a <- as
      b <- bs
    yield a * b)

    def appendAsterisks = augment((s: String, n: Int) => s + "*" * n)
    def g0(sts: Seq[String], ls: Seq[Int]) = sts.mkString(" ") + " " + ls.sum
    def g = augment(g0)

    val (sts1, sts2) = (List("ab", "cd", "ef"), List("gh", "ij", "kl"))
    appendAsterisks crossCheck (sts1, as,
    for
      s <- _
      n <- _
    yield s + "*" * n)

    assertEquals(g(sts1, as), g0(sts1, as))

    g crossCheck (List(sts1, sts2), List(as, bs),
    for
      sts <- _
      ls <- _
    yield g0(sts, ls))

  test("basic C"):
    val (as, bs, cs) = ((3 to 5).toList, 7 to 9, List("acorn", "beetroot"))
    def f0(x: Int, y: Int, s: String) = s"$s: ${x * y}"
    val f = augment(f0)

    f crossCheck (
      as,
      bs,
      cs,
      for
        a <- _
        b <- _
        c <- _
      yield f0(a, b, c)
    )

    f crossCheck (
      as,
      bs,
      cs,
      for
        a <- as
        b <- bs
        c <- cs
      yield f0(a, b, c)
    )

  case class Person(name: String, isMale: Boolean, children: Person*):
    override def toString(): String = s"$name"

  val lara = Person("Lara", false)
  val joe = Person("Joe", true)
  val bob = Person("Bob", true, joe)
  val julie = Person("Julie", false, lara, bob)
  val persons = List(lara, bob, julie)

  test("persons"):
    val getNames = augment((p: Person, c: Person) => (p.name, c.name))

    getNames crossCheck (persons where (!_.isMale), _.children,
    (persons
      .filter: p =>
        !p.isMale)
      .flatMap: p =>
        p.children.map: c =>
          (p.name, c.name))

    getNames crossCheck (persons where (!_.isMale), _.children,
    for
      p <- persons
      if !p.isMale
      c <- p.children
    yield (p.name, c.name))

  def prepend[A] = augment((a: A, l: Seq[A]) => Seq(a) ++ l)

  def permA[A](x: Seq[A]): Seq[Seq[A]] =
    x match
      case Seq() => List(List())
      case _ =>
        prepend(x, a => permA(x -- List(a)))

  def permALC[A](x: Seq[A]): Seq[Seq[A]] =
    x match
      case Seq() => List(List())
      case _ =>
        for
          a <- x
          p <- permA(x -- List(a))
        yield prepend(a, p)

  def permB[A](x: Seq[A]): Seq[Seq[A]] =
    prepend(x, a => permB(x -- List(a))) until x == Seq()

  test("permutations"):
    val s = "ABCD"

    prepend crossCheck (s.toList, (a: Char) => permB(s -- List(a)), permALC(s))

    assertEquals(permA(s), permALC(s))
    assertEquals(permB(s), permALC(s))

  test("permutations unrolled"):
    val l = "ABCD".toList.map(_.toString)
    val s = l.toSet

    def concat0(a: String, b: String, c: String, d: String) = a + b + c + d
    val concatL = augment(concat0)
    val concatS = augment set (concat0)

    val x0 = concatL("a", "b", "c", "d")
    val x1 = concatS("a", "b", "c", "d")
    val y0 = concatL(l, l -- _, l -- _ -- _, l -- _ -- _ -- _)
    val y1 = concatS applyS (s, s - _, s - _ - _, s - _ - _ - _)
    val res = permA("ABCD").map(_.mkString)
    assertEquals(res.length, 24)
    assertEquals(y0, res)
    assertEquals(y1, res.toSet)

  def sq(n: Int) = n * n
  def sqrt(n: Int) = Math.sqrt(n.toDouble).toInt

  def complement[T](l: Iterable[T], full: Seq[T]) =
    full.filter(!l.toList.contains(_))

  def sieve(n: Int) = mult(2 to sqrt(n), 2 to n / _)
  def sieve1(n: Int) = mult(2 to sqrt(n), x => 2 to n / x)

  def primesA(n: Int): Seq[Int] =
    complement(sieve(n), 2 to n)

  def sieveLC(n: Int) =
    for
      x <- 2 to sqrt(n)
      y <- 2 to n / x
    yield x * y

  test("sieve A"):
    val n = 50
    mult crossCheck (2 to sqrt(n), 2 to n / _,
    for
      x <- 2 to sqrt(n)
      y <- 2 to n / x
    yield x * y)

    val primes = complement(sieve(n), 2 to n)
    val primes1 = complement(sieveLC(n), 2 to n)

    assertEquals(primes, primes1)

    mult crossCheck (2 to sqrt(n), 2 to n / _,
    for
      x <- 2 to sqrt(n)
      y <- 2 to n / x
    yield x * y)

  def primesB1(n: Int): Seq[Int] =
    complement(mult(primesB1(sqrt(n)), 2 to n / _), 2 to n) until n == 1

  def primesB2(n: Int): Seq[Int] =
    if n == 1 then Seq()
    else complement(mult(primesB2(sqrt(n)), 2 to n / _), 2 to n)

  def primesBLC(n: Int): Seq[Int] =
    if n == 1 then Seq()
    else
      complement(
        for
          x <- primesBLC(sqrt(n))
          y <- 2 to n / x
        yield x * y,
        2 to n
      )

  def primesC1(n: Int): Seq[Int] =
    complement(mult(primesC1(sqrt(n)), x => x to n / x), 2 to n) until n == 1

  def primesC2(n: Int): Seq[Int] =
    if n == 1 then Seq()
    else complement(mult(primesC1(sqrt(n)), x => x to n / x), 2 to n)

  def primesCLC(n: Int): Seq[Int] =
    if n == 1 then Seq()
    else
      complement(
        for
          x <- primesCLC(sqrt(n))
          y <- x to n / x
        yield x * y,
        2 to n
      )

  test("sieve B/C"):
    val n = 50
    val res = primesA(n)

    assertEquals(primesB1(n), res)
    assertEquals(primesB2(n), res)
    assertEquals(primesBLC(n), res)

    assertEquals(primesC1(n), res)
    assertEquals(primesC2(n), res)
    assertEquals(primesCLC(n), res)

  def queens(n: Int, k: Int = 0): Seq[Seq[Int]] =
    prepend(0 until n, queens(n, k + 1), isSafe) until k == n

  def isSafe(col: Int, queens: Seq[Int]): Boolean =
    val row = queens.length
    val queensWithRow = (row - 1 to 0 by -1) zip queens
    queensWithRow forall { case (r, c) => col != c && math.abs(col - c) != row - r }

  test("queens"):
    val res = queens(8)
    assertEquals(res.length, 92)
    assertEquals(res(0), List(0, 4, 7, 5, 2, 6, 1, 3))

  test("Fibonacci"):
    val plus = augment((x: Int, y: Int) => x + y)
    def fibo1: LazyList[Int] =
      1 #:: 1 #::
        (
          for (a, b) <- fibo1 zip fibo1.tail
          yield a + b
        )
    def fibo2: LazyList[Int] = 1 #:: 1 #:: plus(fibo2 zip fibo2.tail)

    assertEquals(fibo1.take(10), fibo2.take(10))

  test("basic sets"):
    val s1 = HashSet(4, 6, 8, 10)
    val s2 = HashSet(5, 6, 7, 8, 9)
    val multS = augment set ((x: Int, y: Int) => x * y)
    multS crossCheck (s1, s2,
    for
      x <- _
      y <- _
    yield x * y)

  test("comprehension equivalents"):
    val as = 1 to 5
    val bs = (a: Int) => 3 to (a + 5)
    mult crossCheck (as, bs, mult equiv (_, _))
    mult crossCheck (as, bs, mult equiv (as, bs))

  test("precond"):
    val sqrt = augment(
      Math.sqrt,
      Condition(_ >= 0, x => s"Argument to sqrt $x must be positive"),
      Condition(
        // you'd think they'd check this
        (z, x) => if x != 0 then Math.abs(z * z - x) / x <= .000000000001 else z == 0,
        (z, x) => s"Square of square root ($z) should be the same as the original ($x)"
      )
    )
    assertEquals(Try(sqrt(-9)).failed.get.getMessage(), "Argument to sqrt -9.0 must be positive")
    assertEquals(sqrt(9), 3.0)

  test("vectorize"):
    var res = mult(ColVector(1, 4, 5), ColVector(2, 3, 6))
    assertEquals(res, Vector(2, 12, 30))

  test("bubble sort"):
    def swap[T](a: Array[T]) = (i: Int, j: Int) => { val t = a(i); a(i) = a(j); a(j) = t }
    def orderArrayPair[T: Ordering](a: Array[T]) =
      augment((i: Int, j: Int) => if a(j) > a(j + 1) then swap(a)(j, j + 1))

    def bubbleSort[T: Ordering](a: Array[T]) =
      val orderPair = orderArrayPair(a)
      val l = a.length
      orderPair(0 until l, 0 until l - 1 - _)
      a

    assertEquals(bubbleSort("DBECA".toArray).toList, "ABCDE".toList)
    assertEquals(bubbleSort(Array(4, 2, 5, 3, 1)).toList, List(1, 2, 3, 4, 5))

  val board = """
        003020600
        900305001
        001806400
        008102900
        700000008
        006708200
        002609500
        800203009
        005010300
        """.stripMrgn

  def sudoku(a: ArrayB[Int])(bl: Seq[(Int, Int)]): Seq[Seq[Int]] =
    prepend(1 to 9, sudoku(a)(bl drop 1), isValidSudoku(a)(bl)) until bl == Seq()

  def isValidSudoku(a: ArrayB[Int])(bl: Seq[(Int, Int)])(n: Int, y: Seq[Int]) =
    val arr = placedValsToArray(bl.drop(1) zip y, 9, 9)
    // completed board
    val bd = arr.plus(a)
    val ij = bl.head
    val (si, sj) = ((ij._1 - 1) / 3, (ij._2 - 1) / 3)
    val sa = bd.subArray(si, sj, 3, 3)
    val isInSubSquare = sa.exists(_.contains(n))
    val (r, c) = (bd.row(ij._1), bd.col(ij._2))
    val isInRowOrCol = r.contains(n) || c.contains(n)
    !(isInSubSquare || isInRowOrCol)

  def completedBoards(a: ArrayB[Int], bl: Seq[(Int, Int)], values: Seq[Seq[Int]]) =
    val placedVals = values.map(v => bl zip v)
    val arrs = placedVals.map(placedValsToArray(_, 9, 9))
    arrs.map(_.plus(a))

  def placedValsToArray(v: Seq[((Int, Int), Int)], m: Int, n: Int) =
    val mp = v.toMap
    val ll = (1 to n).map(r => (1 to m).map(c => if mp.contains((r, c)) then mp((r, c)) else 0))
    multiArray(1 to n, 1 to n, ll)

  test("sudoku"):
    val bn = stringAsIntSeq(board)
    val a = multiArray(1 to 9, 1 to 9, bn)
    val x = a select (_ == 0)
    val res = sudoku(a)(x)
    val bds = completedBoards(a, x, res)
    assertEquals(bds.length, 1)
    assertEquals(bds(0).orig(0), List(4, 8, 3, 9, 2, 1, 6, 5, 7))
