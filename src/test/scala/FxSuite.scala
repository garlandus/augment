import augmented._
import augmented.given
import augmented.Extensions._
import basicdef._
import mappable._
import mappable.given
import mappable.MappableExtensions._
import shape.SeqExtensions._
import util.{Channel, timed}

import scala.concurrent._
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.math.{abs, pow}
import scala.util._

class FxSuite extends munit.FunSuite:

  val mult = augment((a: Int, b: Int) => a * b)
  val add = augment((a: Int, b: Int, c: Int) => a + b + c)

  test("add/mult options"):
    val x: Option[Int] = Some(4)
    val y: Option[Int] = Some(5)
    val z: Option[Int] = Some(6)

    mult crossCheckFX (x, y,
    for
      a <- x
      b <- y
    yield a * b)

    add crossCheckFX (x.get, y, z.get,
    for
      a <- x
      b <- y
      c <- z
    yield a + b + c)

    assertEquals(4 * 5, 20)
    assertEquals(Some(4) * 5, Some(20))
    assertEquals(4 * None, None)

    assertEquals(4 + 5 + 6, 15)
    assertEquals(Some(4) + 5 + 6, Some(15))
    assertEquals(Some(4) + Some(5) + Some(6), Some(15))
    assertEquals(4 + None + 6, None)

  test("propagate / bubble up"):
    val x0 = 4 * 5
    val y0 = 2 + x0 + 3
    val z0 = 4 * y0
    assertEquals(z0, 100)

    val x = 4 * Some(5)
    val y = 2 + x + 3
    val z = 4 * y
    assertEquals(z, Some(100))
    assertEquals(z.value(), 100)

  test("either"):
    val NaN: Either[String, Int] = Left("NaN")

    assertEquals(4 + 5 + 6, 15)
    assertEquals(Right(4) + 5 + Right(6), Right(15))
    assertEquals(5 + Right(4) + Right(6), Right(15))
    assertEquals(4 + NaN + 6, Left("NaN"))

    val x = 4 * Right(5)
    val y = 2 + x + 3
    val z = 4 * y
    assertEquals(z, Right(100))
    assertEquals(z.value(), 100)

  test("try"):
    val x1 = 4 * Try(1 / 0)
    val y1 = 2 + x1 + 3
    val z1 = 4 * y1
    assert(z1.isFailure)

    val x = Success(4) * 5
    val y = 2 + x + 3
    val z = 4 * y
    assertEquals(z, Success(100))
    assertEquals(z.value(), 100)

  test("logged"):
    val x1 = Logged(4, "a") + Logged(5, "b") + Logged(6, "c")
    val y1 = 4 + 5 + Logged(6, "c")
    assertEquals(x1.value(), 4 + 5 + 6)
    assertEquals(y1.value(), 15)

    val x = 4 * Logged(5, "b")
    val y = 2 + x + 3
    val z = 4 * y
    assertEquals(z.value(), 100)

  def randomWait[A](a: A): A =
    val msWaited = Random.nextInt(100)
    Thread.sleep(msWaited)
    a

  test("random wait"):
    val f = (a: Double, b: Double, c: Double) => (a + b) / c

    val (x, y, z) = (5.0, 7.0, -3.0)
    val r1 = (randomWait(x) + randomWait(y)) / Try(randomWait(z))
    val r2 = (randomWait(x) + Try(randomWait(y))) / randomWait(z)
    assertEquals(r1.value(), -4.0)
    assertEquals(r2.value(), -4.0)

    augment(f) crossCheckFX (randomWait(x), randomWait(y), Try(randomWait(z)),
    for
      a <- Try(randomWait(x))
      b <- Try(randomWait(y))
      c <- Try(randomWait(z))
    yield (a + b) / c)

  test("futures"):
    val futX1 = Future(randomWait(6))
    val futY1 = Future(randomWait(7))
    val futZ1 = Future(randomWait(8))
    val futX2 = Future(randomWait(6))
    val futY2 = Future(randomWait(7))
    val z2 = randomWait(8)

    val f1 = futX2 + futY2 + z2

    val f2 =
      for
        a <- futX1
        b <- futY1
        c <- futZ1
      yield (a + b + c)

    val res = Await.result(f2, Duration(15, SECONDS))
    assertEquals(f1.value(), res)

  test("basic IO"):
    def relDiff(a: Double, b: Double) = abs((a - b) / b)
    val res: BasicIO[Double] =
      sequence(
        5.0,
        Math.sqrt,
        _ + 1,
        _ / 2.0
      )
    def f(d: Double) = pow(d * 2 - 1, 2)
    assert(relDiff(f(res.value()), 5.0) < 0.0000000001)

    val res2: BasicIO[(Double, String)] =
      sequenceAlt(
        (5.0, "Initial value 5, "),
        (x, s) => (Math.sqrt(x), s + "take square root, "),
        (x, s) => (x + 1, s + "add 1, "),
        (x, s) => (x / 2.0, s + "divide by 2")
      )
    assertEquals(res2.value()._1, res.value())
    assertEquals(res2.value()._2, "Initial value 5, take square root, add 1, divide by 2")

  test("basic IO: parallel"):
    val delay = 25
    def f(n: Int) =
      BasicIO(() =>
        Thread.sleep(delay)
        n * 10
      )

    val io1 = f(3) + f(4) + f(5)
    val io2 = image(f(3), f(4), f(5), _ + _ + _)

    val (res1, t1) = timed(io1.value())
    val (res2, t2) = timed(io2.value())
    val isParallel1 = t1 < 2 * delay * 1000
    val isParallel2 = t2 < 2 * delay * 1000

    assertEquals(res1, 120)
    assertEquals(res2, res1)
    assertEquals(isParallel1, true)
    assertEquals(isParallel2, true)

