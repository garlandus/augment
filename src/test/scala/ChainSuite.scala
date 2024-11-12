import augmented._
import augmented.given
import augmented.Extensions._
import basicdef._
import mappable._
import mappable.given
import mappable.MappableExtensions._
import shape.SeqExtensions._

import scala.util.Success
import scala.util.Try
import java.io.IOException

class ChainSuite extends munit.FunSuite:

  val dropN = (s: String, n: Int) => s.drop(n)
  val s = "abcdefghijklmnop"
  val sOpt: Option[String] = Some("abcdefghijklmnop")
  val sOpt1: Option[String] = None

  test("drop n chars"):
    val res1 = s.drop(4)
    val res2 = sOpt.drop(4)
    val res3 = dropN(s, 4)
    assertEquals(res1, res3)
    assertEquals(res1, res2.get)

  test("head of sequence"):
    val sts = List("abcdef", "ghijkl", "mnopqr")
    val res = sts.headE.slice(1, 4)
    assertEquals(res, Right("bcd"))

  test("chains of strings / ints"):
    val l = List("abcd", "xyz 1234", "mnop")

    val res1 = l.drop(1).head
    val res2 = l.drop(1).headOption.split().head
    val res3 = l.drop(1).headE.split().head
    val res4 = l.drop(1).headOption.split().drop(1).head.take(3).toInt
    val res5 = l.drop(1).headE.split().drop(1).head.take(3).toInt
    val res6 = l.drop(1).headE.split().drop(4).head.take(3).toInt
    val res7 = l.drop(1).headE.split().drop(1).head.take(0).toInt
    val res8 = l.drop(1).headOption.split()
    val res9 = l.drop(1).headE.split()
    val res10 = l.drop(4).headE.split().head
    val res11 = l.drop(1).headE.split().drop(5).head.take(3).toInt
    val res12 = l.drop(1).headE.split().head.take(3).toInt
    val res13 = l.drop(1).headOption.split().drop(1).head.slice(1, 3).toInt
    val res14 = l.drop(1).headE.split().drop(1).head.slice(1, 3).toInt

    assertEquals(res1, "xyz 1234")
    assertEquals(res2, Some("xyz"))
    assertEquals(res3, Right("xyz"))
    assertEquals(res4, Some(123))
    assertEquals(res5, Right(123))
    assertEquals(res6.left.get.getClass.getCanonicalName(), "java.util.NoSuchElementException")
    assertEquals(res6.left.get.getMessage(), "Head of empty array")
    assertEquals(res7.left.get.getClass.getCanonicalName(), "java.lang.NumberFormatException")
    assertEquals(res7.left.get.getMessage(), "String not in numerical format: []")
    assertEquals(res8.get.toList, List("xyz", "1234"))
    assertEquals(res9.right.get.toList, List("xyz", "1234"))
    assertEquals(res10.left.get.getClass.getCanonicalName(), "java.util.NoSuchElementException")
    assertEquals(res10.left.get.getMessage(), "head of empty sequence")
    assertEquals(res11.left.get.getClass.getCanonicalName(), "java.util.NoSuchElementException")
    assertEquals(res11.left.get.getMessage(), "Head of empty array")
    assertEquals(res12.left.get.getClass.getCanonicalName(), "java.lang.NumberFormatException")
    assertEquals(res12.left.get.getMessage(), "String not in numerical format: [xyz]")
    assertEquals(res13, Some(23))
    assertEquals(res14, Right(23))

  test("chains of strings / ints with Try"):
    val l = List("abcd", "xyz 1234", "mnop")
    val res1 = Try(l.drop(1)).head
    val res2 = Try(l.drop(1)).head.split().drop(1).head.slice(1, 3).toInt
    assertEquals(res1, Try("xyz 1234"))
    assertEquals(res2, Try(23))

  def safeDiv(a: Int, b: Int) = checked(a / b, b == 0, ArithmeticException(f"Division by zero"))

  test("safe div"):
    val l = List(List("123", "456", "789", "abcd"), List("12", "34", "56"))

    val res1 = l.liftE(0)(8).toInt + safeDiv(5, 2)
    val res2 = l.liftE(0)(3).toInt + safeDiv(5, 2)
    val res3 = l.liftE(0)(1).toInt + safeDiv(5, 1)
    val res4 = l.liftE(0)(1).toInt + safeDiv(5, 0)

    assertEquals(res1.left.get.getClass.getCanonicalName(), "java.lang.IndexOutOfBoundsException")
    assertEquals(res1.left.get.getMessage(), "Out of bounds for sequence of length 4: 8")
    assertEquals(res2.left.get.getClass.getCanonicalName(), "java.lang.NumberFormatException")
    assertEquals(res2.left.get.getMessage(), "String not in numerical format: [abcd]")
    assertEquals(res3, Right(461))
    assertEquals(res4.left.get.getClass.getCanonicalName(), "java.lang.ArithmeticException")
    assertEquals(res4.left.get.getMessage(), "Division by zero")

  test("chains of strings A"):
    val l = List(List("abcd", "xyz 1234", "mnop"), List("hjik", "defg", "012345"))

    val res0 = l.apply(0)
    val res1 = l.lift(0)
    val res2 = l.liftE(1)
    val res3 = l.liftE(1)(2)
    val res4 = l.liftE(4)(2)
    val res5 = l.liftE(0)(7)
    val res6 = l.liftE(1)(2).slice(1, 5).take(3).drop(1)
    val res7 = l.liftE(1)(2).slice(1, 5).take(3).drop(1).head
    val res8 = l.liftE(1)(2).slice(1, 5).take(3).drop(11).head

    assertEquals(res0, List("abcd", "xyz 1234", "mnop"))
    assertEquals(res1, Some(List("abcd", "xyz 1234", "mnop")))
    assertEquals(res2, Right(List("hjik", "defg", "012345")))
    assertEquals(res3, Right("012345"))
    assertEquals(res4.left.get.getClass.getCanonicalName(), "java.lang.IndexOutOfBoundsException")
    assertEquals(res4.left.get.getMessage(), "Out of bounds for sequence of length 2: 4")
    assertEquals(res5.left.get.getClass.getCanonicalName(), "java.lang.IndexOutOfBoundsException")
    assertEquals(res5.left.get.getMessage(), "Out of bounds for sequence of length 3: 7")
    assertEquals(res6, Right("23"))
    assertEquals(res7, Right('2'))
    assertEquals(res8.left.get.getClass.getCanonicalName(), "java.util.NoSuchElementException")
    assertEquals(res8.left.get.getMessage(), "Head of empty string")

  test("chains of strings B"):
    val l = List("abc 12340 def", "ghi", "jkl")

    val a1 = 100 / l.head.split(" ")(1).take(2).toInt + 25
    val a2 = 100 / Right(l).head.split(" ")(1).take(2).toInt + 25
    val a3 = 100 / l.toEitherR.head.split(" ")(1).take(2).toInt + 25
    val a4 = 100 / Option(l).head.split(" ")(1).take(2).toInt + 25
    val a5 = 100 / Try(l).head.split(" ")(1).take(2).toInt + 25

    val a6 = 100 / l.head.split(" ")(1).take(2).toInt.toEither + 25
    val a7 = 100 / l.head.split(" ")(1).take(2).toInt.toOption + 25
    val a8 = 100 / l.head.split(" ")(1).take(2).toInt.toTry + 25

    interceptMessage[java.util.NoSuchElementException]("head of empty list")(
      100 / l.drop(3).head.split(" ")(1).take(2).toInt + 25
    )
    val b2 = 100 / Right(l).drop(3).head.split(" ")(1).take(2).toInt + 25
    val b3 = 100 / l.toEitherR.drop(3).head.split(" ")(1).take(2).toInt + 25
    val b4 = 100 / Option(l).drop(3).head.split(" ")(1).take(2).toInt + 25
    val b5 = 100 / Try(l).drop(3).head.split(" ")(1).take(2).toInt + 25

    val b6 = 100 / l.drop(3).head.split(" ")(1).take(2).toInt.toEither + 25
    val b7 = 100 / l.drop(3).head.split(" ")(1).take(2).toInt.toOption + 25
    val b8 = 100 / l.drop(3).head.split(" ")(1).take(2).toInt.toTry + 25

    interceptMessage[java.lang.ArrayIndexOutOfBoundsException]("Index 5 out of bounds for length 3")(
      100 / l.head.split(" ")(5).take(2).toInt + 25
    )
    val c2 = 100 / Right(l).head.split(" ")(5).take(2).toInt + 25
    val c3 = 100 / l.toEitherR.head.split(" ")(5).take(2).toInt + 25
    val c4 = 100 / Option(l).head.split(" ")(5).take(2).toInt + 25
    val c5 = 100 / Try(l).head.split(" ")(5).take(2).toInt + 25

    val c6 = 100 / l.head.split(" ")(5).take(2).toInt.toEither + 25
    val c7 = 100 / l.head.split(" ")(5).take(2).toInt.toOption + 25
    val c8 = 100 / l.head.split(" ")(5).take(2).toInt.toTry + 25

    interceptMessage[java.lang.NumberFormatException]("For input string: \"de\"")(
      100 / l.head.split(" ")(2).take(2).toInt + 25
    )
    val d2 = 100 / Right(l).head.split(" ")(2).take(2).toInt + 25
    val d3 = 100 / l.toEitherR.head.split(" ")(2).take(2).toInt + 25
    val d4 = 100 / Option(l).head.split(" ")(2).take(2).toInt + 25
    val d5 = 100 / Try(l).head.split(" ")(2).take(2).toInt + 25

    val d6 = 100 / l.head.split(" ")(2).take(2).toInt.toEither + 25
    val d7 = 100 / l.head.split(" ")(2).take(2).toInt.toOption + 25
    val d8 = 100 / l.head.split(" ")(2).take(2).toInt.toTry + 25

    interceptMessage[java.lang.ArithmeticException]("/ by zero")(100 / l.head.split(" ")(1).drop(4).take(1).toInt + 25)
    val e2 = 100 / Right(l).head.split(" ")(1).drop(4).take(1).toInt + 25
    val e3 = 100 / l.toEitherR.head.split(" ")(1).drop(4).take(1).toInt + 25
    val e4 = 100 / Option(l).head.split(" ")(1).drop(4).take(1).toInt + 25
    val e5 = 100 / Try(l).head.split(" ")(1).drop(4).take(1).toInt + 25

    val e6 = 100 / l.head.split(" ")(1).drop(4).take(1).toInt.toEither + 25
    val e7 = 100 / l.head.split(" ")(1).drop(4).take(1).toInt.toOption + 25
    val e8 = 100 / l.head.split(" ")(1).drop(4).take(1).toInt.toTry + 25

    assertEquals(a1, 33)
    assertEquals(a2, Right(33))
    assertEquals(a3, Right(33))
    assertEquals(a4, Some(33))
    assertEquals(a5, Success(33))
    assertEquals(a6, Right(33))
    assertEquals(a7, Some(33))
    assertEquals(a8, Success(33))

    assertEquals(b2.left.get.getClass.getCanonicalName(), "java.util.NoSuchElementException")
    assertEquals(b2.left.get.getMessage(), "Head of empty sequence")
    assertEquals(b3.left.get.getClass.getCanonicalName(), "java.util.NoSuchElementException")
    assertEquals(b3.left.get.getMessage(), "Head of empty sequence")
    assertEquals(b4, None)
    assertEquals(b5.isFailure, true)
    assertEquals(b5.toEither.left.get.getClass.getCanonicalName(), "java.util.NoSuchElementException")
    assertEquals(b5.toEither.left.get.getMessage(), "Head of empty sequence")
    assertEquals(b6.left.get.getClass.getCanonicalName(), "java.util.NoSuchElementException")
    assertEquals(b6.left.get.getMessage(), "head of empty list")
    assertEquals(b7, None)
    assertEquals(b8.isFailure, true)
    assertEquals(b8.toEither.left.get.getClass.getCanonicalName(), "java.util.NoSuchElementException")
    assertEquals(b8.toEither.left.get.getMessage(), "head of empty list")

    assertEquals(c2.left.get.getClass.getCanonicalName(), "java.lang.IndexOutOfBoundsException")
    assertEquals(c2.left.get.getMessage(), "Out of bounds for sequence of length 3: 5")
    assertEquals(c3.left.get.getClass.getCanonicalName(), "java.lang.IndexOutOfBoundsException")
    assertEquals(c3.left.get.getMessage(), "Out of bounds for sequence of length 3: 5")
    assertEquals(c4, None)
    assertEquals(c5.isFailure, true)
    assertEquals(c5.toEither.left.get.getClass.getCanonicalName(), "java.lang.IndexOutOfBoundsException")
    assertEquals(c5.toEither.left.get.getMessage(), "Out of bounds for sequence of length 3: 5")
    assertEquals(c6.left.get.getClass.getCanonicalName(), "java.lang.ArrayIndexOutOfBoundsException")
    assertEquals(c6.left.get.getMessage(), "Index 5 out of bounds for length 3")
    assertEquals(c7, None)
    assertEquals(c8.isFailure, true)
    assertEquals(c8.toEither.left.get.getClass.getCanonicalName(), "java.lang.ArrayIndexOutOfBoundsException")
    assertEquals(c8.toEither.left.get.getMessage(), "Index 5 out of bounds for length 3")

    assertEquals(d2.left.get.getClass.getCanonicalName(), "java.lang.NumberFormatException")
    assertEquals(d2.left.get.getMessage(), "String not in numerical format: [de]")
    assertEquals(d3.left.get.getClass.getCanonicalName(), "java.lang.NumberFormatException")
    assertEquals(d3.left.get.getMessage(), "String not in numerical format: [de]")
    assertEquals(d4, None)
    assertEquals(d5.isFailure, true)
    assertEquals(d5.toEither.left.get.getClass.getCanonicalName(), "java.lang.NumberFormatException")
    assertEquals(d5.toEither.left.get.getMessage(), "String not in numerical format: [de]")
    assertEquals(d6.left.get.getClass.getCanonicalName(), "java.lang.NumberFormatException")
    assertEquals(d6.left.get.getMessage(), "For input string: \"de\"")
    assertEquals(d7, None)
    assertEquals(d8.isFailure, true)
    assertEquals(d8.toEither.left.get.getClass.getCanonicalName(), "java.lang.NumberFormatException")
    assertEquals(d8.toEither.left.get.getMessage(), "For input string: \"de\"")

    assertEquals(e2.left.get.getClass.getCanonicalName(), "java.lang.ArithmeticException")
    assertEquals(e2.left.get.getMessage(), "division by zero")
    assertEquals(e3.left.get.getClass.getCanonicalName(), "java.lang.ArithmeticException")
    assertEquals(e3.left.get.getMessage(), "division by zero")
    assertEquals(e4, None)
    assertEquals(e5.isFailure, true)
    assertEquals(e5.toEither.left.get.getClass.getCanonicalName(), "java.lang.ArithmeticException")
    assertEquals(e5.toEither.left.get.getMessage(), "division by zero")
    assertEquals(e6.left.get.getClass.getCanonicalName(), "java.lang.ArithmeticException")
    assertEquals(e6.left.get.getMessage(), "division by zero")
    assertEquals(e7, None)
    assertEquals(e8.isFailure, true)
    assertEquals(e8.toEither.left.get.getClass.getCanonicalName(), "java.lang.ArithmeticException")
    assertEquals(e8.toEither.left.get.getMessage(), "division by zero")
