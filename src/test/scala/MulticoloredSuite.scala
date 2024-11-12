import augmented._
import augmented.given
import augmented.Extensions._
import basicdef._
import mappable._
import mappable.given
import mappable.MappableExtensions._
import shape.SeqExtensions._

import java.time.LocalDate

class MulticoloredSuite extends munit.FunSuite:

  var getNumber = (b: Boolean) =>
    if b then -1.0
    else throw IllegalArgumentException("number not provided")

  var sqrt = (x: Double) => Math.sqrt(x)

  var calcRoot = (a: Double, b: Double, c: Double) => .5 * (-b + sqrt(b * b - 4 * a * c))

  val (a, c) = (1, -1)
  val ratio = (1 + Math.sqrt(5)) / 2

  test("find root A1"):
    val b = getNumber(true)
    val res = calcRoot(a, b, c)
    assertEquals(res, ratio)

  test("find root A2"):
    intercept[java.lang.IllegalArgumentException] {
      val b = getNumber(false)
      val res = calcRoot(a, b, c)
      val ratio = (1 + Math.sqrt(5)) / 2
    }

  test("find root A3"):
    val res = calcRoot(1, 1, 1)
    assertEquals(res.isNaN(), true)

  var getNumberB = (b: Boolean) =>
    if b then Right(-1.0)
    else Left(java.io.IOException())

  test("find root B1"):
    val b = getNumberB(true)
    val res = calcRoot(a, b, c)
    assertEquals(res.value(), ratio)

  test("find root B2"):
    val b = getNumberB(false)
    val res = calcRoot(a, b, c)
    assertEquals(res.isLeft, true)
    assertEquals(res.left.get.getClass.getCanonicalName(), "java.io.IOException")

  test("find root B3"):
    val res = calcRoot(1, 1, 1)
    assertEquals(res.isNaN(), true)

  var getNumberC = (b: Boolean) =>
    if b then Right(-1.0)
    else Left(java.io.IOException())

  var sqrtC = (x: Double) => if x >= 0 then Right(Math.sqrt(x)) else Left(java.lang.ArithmeticException(x.toString()))

  var calcRootC = (a: Double, b: Double, c: Double) => .5 * (-b + sqrtC(b * b - 4 * a * c))

  test("find root C1"):
    val b = getNumberC(true)
    val res = calcRootC(a, b, c)
    assertEquals(res.value(), ratio)

  test("find root C2"):
    val b = getNumberC(false)
    val res = calcRootC(a, b, c)
    assertEquals(res.isLeft, true)
    assertEquals(res.left.get.getClass.getCanonicalName(), "java.io.IOException")

  test("find root C3"):
    val b = getNumberC(true)
    val res = calcRootC(1, 1, 1)
    assertEquals(res.isLeft, true)
    assertEquals(res.left.get.getClass.getCanonicalName(), "java.lang.ArithmeticException")

  case class User(name: String, dob: LocalDate)

  val sq = (i: Int) => i * i
  val f1 = (user: User) => f"Name: ${user.name} | DOB: ${user.dob}"
  val f2 = (a: Int, b: Int) => a * b + 100
  val f3 = (a: Int, b: Boolean) => a + (if b then 100 else 50)
  val f4 = (a: Int, dt: LocalDate) => a + 100
  val f5 = (s: String, dt: LocalDate) => f"$s $dt"
  val f6 = (b: Boolean, dt: LocalDate) => f"$b $dt"
  val f7 = (i: Int, s: String, u: User) => f"($i) $s | ${u.name}"
  val f8 = (i: Int, s: String, u: User) =>
    if i == 0 then None
    else Some(f"($i) $s | ${u.name}")

  val aOpt: Option[Int] = Some(12)
  val bOpt: Option[Int] = Some(5)

  val aEith: Either[String, Int] = Right(10)
  val bEith: Either[String, Int] = Right(25)
  val cEith: Either[String, Int] = Left("Fred")
  val dEith: Either[Exception, Int] = Right(10)

  val saOpt: Option[String] = Some("The 20th century started on: ")
  val sbOpt: Option[String] = None
  val dtaOpt: Option[LocalDate] = Some(LocalDate.of(1900, 1, 1))
  val dtbOpt: Option[LocalDate] = None

  val saEith: Either[Exception, String] = Right("A historical date is: ")
  val sbEith: Either[Exception, String] = Left(IllegalArgumentException("Date not recognized"))
  val dtEith: Either[Exception, LocalDate] = Right(LocalDate.of(1900, 1, 1))

  val user = User("Fred", LocalDate.of(2000, 4, 1))
  val useraOpt: Option[User] = Some(user)
  val userbOpt: Option[User] = None
  val useraEith: Either[Exception, User] = Right(user)
  val userbEith: Either[Exception, User] = Left(IllegalArgumentException("Unknown user"))

  test("basic A1"):
    given Atomic[User] with
      def tag = "User"

    val res1 = f1(user)
    val res2 = f1(useraOpt)
    val res3 = f1(userbOpt)
    assertEquals(res1, "Name: Fred | DOB: 2000-04-01")
    assertEquals(res2, Some("Name: Fred | DOB: 2000-04-01"))
    assertEquals(res3, None)
    val res4 = sq(aOpt)
    assertEquals(res4, Some(144))

  test("basic B1"):
    val res1 = f2(5, 6)
    val res2 = f2(aOpt, bOpt)
    val res3 = f2(aEith, bEith)
    val res4 = f2(aEith, cEith)
    assertEquals(res1, 130)
    assertEquals(res2, Some(160))
    assertEquals(res3, Right(350))
    assertEquals(res4, Left("Fred"))

  test("basic B1a"):
    val res1 = f2(5, 6)
    val res2 = f2(1, bOpt)
    val res3 = f2(aOpt, 5)
    val res4 = f2(aEith, 4)
    val res5 = f2(6, cEith)
    assertEquals(res1, 130)
    assertEquals(res2, Some(105))
    assertEquals(res3, Some(160))
    assertEquals(res4, Right(140))
    assertEquals(res5, Left("Fred"))

  test("basic B2"):
    val res1 = f5("At the turn of the last century: ", LocalDate.of(1900, 1, 1))
    val res2 = f5(saOpt, dtaOpt)
    val res3 = f5(sbOpt, dtaOpt)
    val res4 = f5(saOpt, dtbOpt)
    val res5 = f5(saEith, dtEith)
    val res6 = f5(sbEith, dtEith)
    val res7 = f6(Option(true), dtaOpt)
    val res8 = f2(Option(1), Option(4))
    val res9 = f3(Option(1), Option(false))
    val res10 = f4(Option(1), Option(LocalDate.of(1900, 1, 1)))
    val res11 = f6(Option(true), Option(LocalDate.of(1900, 1, 1)))
    assertEquals(res1, "At the turn of the last century:  1900-01-01")
    assertEquals(res2, Some("The 20th century started on:  1900-01-01"))
    assertEquals(res3, None)
    assertEquals(res4, None)
    assertEquals(res5, Right("A historical date is:  1900-01-01"))
    assertEquals(res6.left.get.getMessage(), "Date not recognized")
    assertEquals(res7, Some("true 1900-01-01"))
    assertEquals(res8, Some(104))
    assertEquals(res9, Some(51))
    assertEquals(res10, Some(101))
    assertEquals(res11, Some("true 1900-01-01"))

  test("basic C1"):
    val res1 = f7(5, "Beanstalk", User("Jack", LocalDate.of(2000, 4, 1)))
    val res2 = f7(aOpt, saOpt, useraOpt)
    val res3 = f7(aOpt, sbOpt, useraOpt)
    val res4 = f7(bOpt, saOpt, userbOpt)
    val res5 = f7(dEith, saEith, useraEith)
    val res6 = f7(dEith, saEith, useraEith)
    val res7 = f7(dEith, saEith, userbEith)
    val res8 = f7(dEith, sbEith, useraEith)
    assertEquals(res1, "(5) Beanstalk | Jack")
    assertEquals(res2, Some("(12) The 20th century started on:  | Fred"))
    assertEquals(res3, None)
    assertEquals(res4, None)
    assertEquals(res5, Right("(10) A historical date is:  | Fred"))
    assertEquals(res6, Right("(10) A historical date is:  | Fred"))
    assertEquals(res7.left.get.getMessage(), "Unknown user")
    assertEquals(res8.left.get.getMessage(), "Date not recognized")

  test("basic C2"):
    val res1 = f7(5, "Beanstalk", User("Jack", LocalDate.of(2000, 4, 1)))
    val res2 = f7(4, saOpt, useraOpt)
    val res4 = f7(bOpt, saOpt, user)
    val res5 = f7(10, saEith, useraEith)
    val res7 = f7(dEith, saEith, user)
    val res8 = f7(dEith, sbEith, user)
    val res9 = f7(10, "Beanstalk", userbEith)
    assertEquals(res1, "(5) Beanstalk | Jack")
    assertEquals(res2, Some("(4) The 20th century started on:  | Fred"))
    assertEquals(res4, Some("(5) The 20th century started on:  | Fred"))
    assertEquals(res5, Right("(10) A historical date is:  | Fred"))
    assertEquals(res7, Right("(10) A historical date is:  | Fred"))
    assertEquals(res8.left.get.getMessage(), "Date not recognized")
    assertEquals(res9.left.get.getMessage(), "Unknown user")

  test("basic C3"):
    val res1 = f8(5, "Beanstalk", user)
    val res2 = f8(4, saOpt, useraOpt)
    val h2 = augment(f8)
    val h3 = augment.applyA(f8)
    val res3 = h2(5, "Beanstalk", user)
    val res4 = h2(4, saOpt, useraOpt)
    val res5 = h3(4, saOpt, useraOpt)
    assertEquals(res1, Some("(5) Beanstalk | Fred"))
    assertEquals(res2, Some("(4) The 20th century started on:  | Fred"))
    assertEquals(res3, Some("(5) Beanstalk | Fred"))
    assertEquals(res5, Some("(4) The 20th century started on:  | Fred"))
