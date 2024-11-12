import augmented._
import augmented.given
import augmented.Extensions._
import basicdef._
import mappable._
import mappable.given
import mappablethirdparty.given

import scala.concurrent._
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.Success

import cats.data._
import cats.syntax.traverse._

class ImplicitHofSuite extends munit.FunSuite:

  val aOpt = Option(5)
  val l = List(1, 4, 5, 8, 17)
  val isEven = (x: Int) => x % 2 == 0
  val plus = (a: Int, b: Int) => a + b
  val increment = plus(1, _)

  test("implicit filter"):

    val res = isEven(l)

    assertEquals(res, l.filter(isEven))
    assertEquals(res, List(4, 8))
    assertEquals(augment(isEven)(l), res)

  test("implicit fold"):

    val res: Int = plus(l)

    assertEquals(res, l.drop(1).fold(l.head)(plus))
    assertEquals(res, l.sum)
    assertEquals(res, 35)
    assertEquals(augment(plus)(l), res)

  test("implicit map"):

    val res1 = augment(increment)(l).toList
    assertEquals(res1, l.map(increment))
    assertEquals(res1, List(2, 5, 6, 9, 18))

    val res2 = increment(aOpt)
    assertEquals(res2, aOpt.map(increment))
    assertEquals(res2, Option(6))
    assertEquals(augment(increment)(aOpt), res2)

  test("implicit sequence"):

    val plus = (a: Int, b: Int) => a + b
    val l1 = List(Option(5), Option(3), Option(10))
    val l2 = List(Option(5), None, Option(10))

    val res1 = plus(l1)
    val res2 = plus(l2)
    assertEquals(res1, l1.sequence.map(_.sum))
    assertEquals(res2, l2.sequence.map(_.sum))
    assertEquals(res1, Some(18))
    assertEquals(res2, None)

  val hostnames = List(
    "alpha.example.com",
    "beta.example.com",
    "gamma.demo.com"
  )

  def getUptime = (hostname: String) => Future(hostname.length * 60)
  def getUptimeOpt = (hostname: String) => Option(hostname.length * 60)

  def getUptimeFutEith(pred: String => Boolean) = (hostname: String) =>
    val res = if pred(hostname) then Right(hostname.length * 60) else Left(f"Not this one: $hostname")
    Future(res)

  val uptimes = List(1020, 960, 840)

  test("implicit traversal"):

    val res1 = getUptime(hostnames)
    val res2 = getUptimeOpt(hostnames)
    val res3 = traversal(hostnames)(getUptime)

    Thread.sleep(25)
    assertEquals(res1.value.get.get, uptimes)
    assertEquals(res1.value.get, res3.value.get)

    assertEquals(res2, traversal(hostnames)(getUptimeOpt))
    assertEquals(res2, Some(uptimes))

    val getUptimeOpt1 = augment(getUptimeOpt)
    assertEquals(getUptimeOpt1(hostnames), res2)

  test("implicit monad transformer + traversal"):

    val getUptimes1 = getUptimeFutEith(!_.startsWith("Z"))
    val getUptimes2 = getUptimeFutEith(!_.startsWith("g"))

    val res1: Future[Either[String, List[Int]]] = getUptimes1(hostnames)
    val res2: Future[Either[String, List[Int]]] = getUptimes2(hostnames)

    val res3a = hostnames.map(getUptimes1)
    val res4a = hostnames.map(getUptimes2)

    Thread.sleep(25)

    assertEquals(res1.value.get, Success(Right(uptimes)))
    assertEquals(res2.value.get, Success(Left("Not this one: gamma.demo.com")))

    val res3 = res3a.traverse(EitherT(_)).value
    val res4 = res4a.traverse(EitherT(_)).value

    Thread.sleep(25)

    assertEquals(res3.value.get, res1.value.get)
    assertEquals(res4.value.get, res2.value.get)

  case class User(name: String)
  case class Order(id: Int, qty: Int)

  val userId = 1

  def fetchUser   = (id: Int) => if id == 1 then Some(User("John")) else None
  def fetchOrder  = (user: User) => if user.name == "John" then Some(Order(10, 100)) else None

  def fetchUser1  = (id: Int) => Future(fetchUser(id))
  def fetchOrder1 = (user: User) => Future(fetchOrder(user))

  test("implicit monad transformer"):
    
    val order1 = fetchOrder(fetchUser(userId))
    val order2 = fetchOrder1(fetchUser1(userId))
    val order3 = Future(Option(Order(10, 100)))
    Thread.sleep(25)
    val orderValue = order3.value
    assertEquals(order1, Option(Order(10, 100)))
    assertEquals(order2.value, orderValue)

  test("implicit monad transformer: options in futures"):

    /** Taken from https://youtu.be/hGMndafDcc8?t=260 */

    def getX = Future(Option(5))
    def getY = Future(Option(3))
    val add = (a: Int, b: Int) => a + b

    val res1 =
      for
        x <- OptionT(getX)
        y <- OptionT(getY)
      yield x + y

    val res2 = add(getX, getY)

    Thread.sleep(25)
    assertEquals(res1.value.value.get.get, res2.value.get.get)
    assertEquals(res1.value.value.get.get, Some(8))

  test("implicit monad transformer: options in futures (II)"):

    def add = (a: Int, b: Int, c: Int) => a + b + c

    def age = (name: String) =>
      Future(
        name match
          case "Erik" => Right(30)
          case other  => Left(f"Age of $other is unknown")
      )

    val r1 = age("Erik")
    val r2 = age("John")

    val totalAge =
      for
        a <- EitherT(age("Erik"))
        b <- EitherT(age("John"))
        c <- EitherT(age("Jane"))
      yield a + b + c

    val a = age("Erik")
    val b = age("John")
    val c = age("Jane")
    val totalAge1 = add(a, b, c)

    Thread.sleep(25)
    assertEquals(totalAge.value.value.get.get, totalAge1.value.get.get)
    assertEquals(totalAge.value.value.get.get, Left("Age of John is unknown"))
