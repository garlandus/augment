import augmented._
import augmented.given
import augmented.Extensions._
import basicdef._
import mappable._
import mappable.given
import mappablethirdparty.given

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.Success
import scala.util.Try
import cats.data.EitherT
import cats.syntax.all._
import zio._
import zio.Console.*
import java.io.IOException

class ExternalExampleSuite extends munit.FunSuite:

  /** Taken from https://alvinalexander.com/video-course/functional-programming-2/a-small-zio-2-example-application/
    * (3:30)
    */

  def makeInt(s: String): ZIO[Any, NumberFormatException, Int] =
    ZIO
      .attempt(s.toInt)
      .refineToOrDie[NumberFormatException]

  def makeIntOpt(s: String): Option[Int] = s.toIntOption

  test("equation pseudo-code"):

    /** "just pseudo-code" */
    val equation =
      val a = makeInt("1")
      val b = makeInt("uh oh")
      val c = makeInt("3")
      a + b + c

    val equation1 = makeInt("1") + makeInt("uh oh") + makeInt("3")

    def sum = (a: Int, b: Int, c: Int) => a + b + c
    val equation2 = sum(makeInt("1"), makeInt("uh oh"), makeInt("3"))

    val equation3 =
      for
        a <- makeInt("1")
        b <- makeInt("uh oh")
        c <- makeInt("3")
      yield a + b + c

    def resultStr[R, E, A](equ: ZIO[R, E, A]) = equ.fold(
      failure => s"Failure: $failure",
      success => s"Success: $success"
    )

    val msg1 = "Failure: java.lang.NumberFormatException: For input string: \"uh oh\""
    assertEquals(resultStr(equation).value(), msg1)
    assertEquals(resultStr(equation1).value(), msg1)
    assertEquals(resultStr(equation2).value(), msg1)
    assertEquals(resultStr(equation3).value(), msg1)

    val msg2 = "For input string: \"uh oh\""
    assertEquals(equation.result().left.get.getMessage(), msg2)
    assertEquals(equation1.result().left.get.getMessage(), msg2)
    assertEquals(equation2.result().left.get.getMessage(), msg2)
    assertEquals(equation3.result().left.get.getMessage(), msg2)
    interceptMessage[zio.FiberFailure](msg2)(equation.value())
    interceptMessage[zio.FiberFailure](msg2)(equation1.value())
    interceptMessage[zio.FiberFailure](msg2)(equation2.value())
    interceptMessage[zio.FiberFailure](msg2)(equation2.value())

    val equation4 =
      val a = makeInt("1")
      val b = makeInt("2")
      val c = makeInt("3")
      a + b + c

    val equation5 = makeInt("1") + makeInt("2") + makeInt("3")
    val equation6 = sum(makeInt("1"), makeInt("2"), makeInt("3"))

    val equation7 =
      for
        a <- makeInt("1")
        b <- makeInt("2")
        c <- makeInt("3")
      yield a + b + c

    assertEquals(equation4.value(), 6)
    assertEquals(equation5.value(), 6)
    assertEquals(equation6.value(), 6)
    assertEquals(equation7.value(), 6)

  /** Taken from https://www.youtube.com/watch?v=8Mw5oJRc2Dc&t=909s */

  def fahrenheitToCelsius(fahrenheit: Double): Double =
    (fahrenheit - 32) * 5 / 9

  def getUserInput: ZIO[Any, IOException, String] =
    printLine("\nEnter temperature in Fahrenheit: ")
    ZIO.succeed("68.0")

  def parseInput(input: String): ZIO[Any, NumberFormatException, Double] =
    ZIO.attempt(input.toDouble).refineToOrDie[NumberFormatException]

  def displayResult(celsius: Double): ZIO[Any, IOException, Double] =
    // printLine(f"Temperature in Celsius: $celsius%.2f C")
    ZIO.succeed(celsius)

  test("fahrenheit"):

    def prog =
      for
        input <- getUserInput
        fahrenheit <- parseInput(input)
        celsius = fahrenheitToCelsius(fahrenheit)
        _ <- displayResult(celsius)
      yield celsius

    def tempZ =
      sequence(
        getUserInput,
        parseInput,
        fahrenheitToCelsius,
        displayResult
      )

    val temp1 = prog.value()
    val temp2 = tempZ.value()
    assertEquals(temp1, 20.0)
    assertEquals(temp2, 20.0)

  /** Taken from https://typelevel.org/cats/datatypes/eithert.html */

  def dividePlain = (a: Double, b: Double) => a / b
  def parseDoublePlain = (s: String) => s.toDouble

  def parseDouble(s: String): Either[String, Double] =
    Try(s.toDouble).map(Right(_)).getOrElse(Left(s"$s is not a number"))

  def divide(a: Double, b: Double): Either[String, Double] =
    Either.cond(b != 0, a / b, "Cannot divide by zero")

  def parseDoubleAsync(s: String): Future[Either[String, Double]] =
    Future.successful(parseDouble(s))

  def divideAsync(a: Double, b: Double): Future[Either[String, Double]] =
    Future.successful(divide(a, b))

  test("division"):

    def divisionProgram(inputA: String, inputB: String): Either[String, Double] =
      for {
        a <- parseDouble(inputA)
        b <- parseDouble(inputB)
        result <- divide(a, b)
      } yield result

    def division(inputA: String, inputB: String): Either[String, Double] =
      (divide _)(parseDouble(inputA), parseDouble(inputB))

    val res1 = divisionProgram("10", "2")
    val res2 = divisionProgram("a", "b")

    val res3 = division("10", "2")
    val res4 = division("a", "b")

    assertEquals(res1, Right(5.0))
    assertEquals(res2, Left("a is not a number"))
    assertEquals(res3, Right(5.0))
    assertEquals(res4, Left("a is not a number"))

  test("division (async) A"):

    def divisionProgramAsync(inputA: String, inputB: String): EitherT[Future, String, Double] =
      for {
        a <- EitherT(parseDoubleAsync(inputA))
        b <- EitherT(parseDoubleAsync(inputB))
        result <- EitherT(divideAsync(a, b))
      } yield result

    def division(inputA: String, inputB: String) =
      (divideAsync _) (parseDoubleAsync (inputA), parseDoubleAsync (inputB))

    val res1 = divisionProgramAsync("10", "2").value
    val res2 = divisionProgramAsync(
      "a",
      "b"
    ).value
    val res3 = division("10", "2")
    val res4 = division("a", "b")

    Thread.sleep(25)

    assertEquals(res1.value.get.isSuccess, true)
    assertEquals(res1.value.get.get, Right(5.0))
    assertEquals(res2.value.get.isSuccess, true)
    assertEquals(res2.value.get.get, Left("a is not a number"))
    assertEquals(res3.value.get.isSuccess, true)
    assertEquals(res3.value.get.get, Right(5.0))
    assertEquals(res4.value.get.isSuccess, true)
    assertEquals(res4.value.get.get, Left("a is not a number"))

  test("division (async) B"):

    val div: (Double, Double) => Double = _ / _
    val divide1: (Double, Double) => Double = (a: Double, b: Double) => a / b
    val divide2: (Double, Double) => Either[String, Double] = divide
    val divide3: (Double, Double) => Future[Either[String, Double]] = divideAsync

    val parseDbl: String => Double = _.toDouble
    val parseDbl1: String => Double = (s: String) => s.toDouble
    val parseDbl2: String => Either[String, Double] = parseDouble
    val parseDbl3: String => Future[Either[String, Double]] = parseDoubleAsync

    def division0(inputA: String, inputB: String) = div(parseDbl(inputA), parseDbl(inputB))

    def division11(inputA: String, inputB: String) = divide1(parseDbl1(inputA), parseDbl1(inputB))
    def division12(inputA: String, inputB: String) = divide1(parseDbl2(inputA), parseDbl2(inputB))
    def division13(inputA: String, inputB: String) = divide1(parseDbl3(inputA), parseDbl3(inputB))

    def division21(inputA: String, inputB: String) = divide2(parseDbl1(inputA), parseDbl1(inputB))
    def division22(inputA: String, inputB: String) = divide2(parseDbl2(inputA), parseDbl2(inputB))

    def division31(inputA: String, inputB: String) = divide3(parseDbl1(inputA), parseDbl1(inputB))
    def division33(inputA: String, inputB: String) = divide3(parseDbl3(inputA), parseDbl3(inputB))

    val res11a = division11("10", "2")
    val res11c = division11("10", "0")

    val res12a = division12("10", "2")
    val res12b = division12("a", "b")
    val res12c = division12("10", "0")

    val res13a = division13("10", "2")
    val res13b = division13("a", "b")
    val res13c = division13("10", "0")

    val res21a = division21("10", "2")
    val res21c = division21("10", "0")

    val res22a = division22("10", "2")
    val res22b = division22("a", "b")
    val res22c = division22("10", "0")

    val res31a = division31("10", "2")
    val res31c = division31("10", "0")

    val res33a = division33("10", "2")
    val res33b = division33("a", "b")
    val res33c = division33("10", "0")

    Thread.sleep(25)

    assertEquals(res11a, 5.0)
    interceptMessage[java.lang.NumberFormatException]("For input string: \"a\"")(division11("a", "b"))
    assertEquals(res11c.isPosInfinity, true)

    assertEquals(res12a, Right(5.0))
    assertEquals(res12b, Left("a is not a number"))
    assertEquals(res12c.right.get.isPosInfinity, true)

    assertEquals(res13a.value.get.get, Right(5.0))
    assertEquals(res13b.value.get.get, Left("a is not a number"))
    assertEquals(res13c.value.get.get.right.get.isPosInfinity, true)

    assertEquals(res21a, Right(5.0))
    interceptMessage[java.lang.NumberFormatException]("For input string: \"a\"")(division21("a", "b"))
    assertEquals(res21c, Left("Cannot divide by zero"))

    assertEquals(res22a, Right(5.0))
    assertEquals(res22b, Left("a is not a number"))
    assertEquals(res22c, Left("Cannot divide by zero"))

    assertEquals(res31a.value.get.get, Right(5.0))
    interceptMessage[java.lang.NumberFormatException]("For input string: \"a\"")(division31("a", "b"))
    assertEquals(res31c.value.get.get, Left("Cannot divide by zero"))

    assertEquals(res33a.value.get.get, Right(5.0))
    assertEquals(res33b.value.get.get, Left("a is not a number"))
    assertEquals(res33c.value.get.get, Left("Cannot divide by zero"))
