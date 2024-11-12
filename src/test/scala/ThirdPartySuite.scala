import augmented._
import augmented.given
import augmented.Extensions._
import basicdef._
import comprehension._
import comprehension.given
import mappable._
import mappable.given
import mappablethirdparty._
import mappablethirdparty.given
import shape._
import util._

import cats.data.State._
import cats.syntax.all._
import com.google.common.collect._
import java.io.IOException
import org.apache.commons.math3.util._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.io.StdIn.readLine
import scala.util.{Random, Try}
import zio.direct._
import zio.ZIO

class ThirdPartySuite extends munit.FunSuite:

  def printSeq[A](seq: Seq[Seq[A]]) =
    println(s"${seq.map(_.mkString(" ")).mkString("\n")}\n")

  test("Pascal's triangle"):
    val binomialCoefficient = augment seq (CombinatoricsUtils.binomialCoefficient)

    val res0 = binomialCoefficient(7, 3)
    assertEquals(res0, 35L)

    def PascalsTriangle(n: Int) = binomialCoefficient(0 to n, 0 to _)

    val res = PascalsTriangle(5)
    assertEquals(res.map(_.length).sum, 21)

  test("Guava table / multiset"):
    given AugmentB[Table, SeqNB] = AugmentB()
    given AugmentC[SeqNC, GuavaMultiSet] = AugmentC()

    val mult = augment((a: Int, b: Int) => a * b)
    val res1 = mult(List(10, 100), List(2, 3, 5))

    assertEquals(
      res1.columnMap(),
      java.util.Map.of(
        10,
        java.util.Map.of(20, 2, 30, 3, 50, 5),
        100,
        java.util.Map.of(200, 2, 300, 3, 500, 5)
      )
    )

    val n = 30
    val res2 = select(1 to n, _ to n, _ to n, (a, b, c) => a * a + b * b == c * c)

    assertEquals(
      res2.elementSet(),
      java.util.Set.of(
        (3, 4, 5),
        (5, 12, 13),
        (6, 8, 10),
        (7, 24, 25),
        (8, 15, 17),
        (9, 12, 15),
        (10, 24, 26),
        (12, 16, 20),
        (15, 20, 25),
        (18, 24, 30),
        (20, 21, 29)
      )
    )

  test("ZIO basic"):

    def getName() =
      Thread.sleep(25)
      "Fred"
    val r: ZIO[Any, IOException, String] =
      sequence(
        println("What is your name?"),
        getName(),
        name => { println(s"Hello, $name"); name }
      )

    assertEquals(r.value(), getName())

  val add = augment((a: Int, b: Int, c: Int) => a + b + c)
  val mult = augment flat ((a: Int, b: Int, c: Int) => a * b * c)

  test("ZIO numeric"):
    val res1 = ZIO.succeed(4) + ZIO.succeed(5)
    val res2 = ZIO.succeed(4) * 5
    val res3 = 4 * ZIO.succeed(5) + ZIO.succeed(6)
    assertEquals(res1.value(), 9)
    assertEquals(res2.value(), 20)
    assertEquals(res3.value(), 26)


  test("Cats applicatives"):
    val o1 = Some(1).asInstanceOf[Option[Int]]
    val o2 = Some(2)
    val o3 = Some(3)
    val res1 = (o1, o2, o3).mapN { _ + _ + _ }
    val res2 = (o1, o2, o3).mapN(add.apply)
    val res3 = o1 + o2 + o3
    val res4 = 1 + 2 + o3
    val res5 = 1 + None + o3

    assertEquals(res1, res2)
    assertEquals(res1, res3)
    assertEquals(res3, res4)

    val res6 = (List(1, 2), List(3, 4), List(0, 2)).mapN(_ * _ * _)
    val res7 = mult(List(1, 2), List(3, 4), List(0, 2)).toList

    assertEquals(res6, res7)

  test("Cats logged"):
    type CatsLogged[A] = cats.data.Writer[Vector[String], A]
    val logged1 =
      for
        a <- 10.pure[CatsLogged]
        _ <- Vector("a", "b", "c").tell
        b <- 32.writer(Vector("x", "y", "z"))
      yield a + b

    def addB[B] = (a: Int, b: B, c: Int) => a + c

    assertEquals(addB(10, "ignore", 32), 42)
    assertEquals(addB(10, (), 32), 42)

    val logged2 =
      addB(
        10.pure[CatsLogged],
        Vector("a", "b", "c").tell,
        32.writer(Vector("x", "y", "z"))
      )

    val logged3 = 10.pure[CatsLogged] + 0.writer(Vector("a", "b", "c")) + 32.writer(Vector("x", "y", "z"))

    assertEquals(logged2.toString(), "WriterT((Vector(a, b, c, x, y, z),42))")
    assertEquals(logged1, logged2)
    assertEquals(logged1, logged3)

  type Stack[A] = List[A]
  def pop[A] = cats.data.State[Stack[A], A]:
    case x :: xs => (xs, x)
    case Nil     => throw Exception("empty stack")
  def push[A](a: A) = cats.data.State[Stack[A], Unit]:
    case xs => (a :: xs, ())

  test("Cats state"):
    val stack: Stack[Int] = List(3, 4, 5)

    val st1: cats.data.State[Stack[Int], ?] =
      for
        a <- pop
        b <- pop
        _ <- push(a * b)
      yield ()

    val st2: cats.data.State[Stack[Int], ?] =
      sequence(
        pop,
        pop,
        (a, b) => push(a * b)
      )

    val st3: cats.data.State[Stack[Int], ?] =
      sequence(
        pop,
        pop,
        _ * _,
        push(_)
      )

    assertEquals(st1.run(stack).value._1.head, 12)
    assertEquals(st2.run(stack).value._1.head, 12)
    assertEquals(st3.run(stack).value._1.head, 12)

    val st4: cats.data.State[Stack[Int], ?] =
      for
        a <- pop
        b <- pop
        c <- pop
        _ <- push(a * b + c)
      yield ()

    val st5: cats.data.State[Stack[Int], ?] =
      sequence(
        pop,
        pop,
        pop,
        _ * _ + _,
        push(_)
      )

    assertEquals(st4.run(stack).value._1.head, 17)
    assertEquals(st5.run(stack).value._1.head, 17)

    val st6: cats.data.State[Stack[Int], ?] =
      for
        _ <- modify[Stack[Int]](_.map(_ * 10))
        a <- pop
        b <- pop
        _ <- push(a + b)
      yield ()

    val st7: cats.data.State[Stack[Int], ?] =
      sequence(
        modify[Stack[Int]](_.map(_ * 10)),
        pop,
        pop,
        (_, x, y) => x + y,
        push(_)
      )

    assertEquals(st6.run(stack).value._1.head, 70)
    assertEquals(st7.run(stack).value._1.head, 70)

  val c1 = Channel[Int]("c1")
  val c2 = Channel[Int]("c2")
  val c3 = Channel[Int]("c3")
  val ch = Channel[String]("ch")

  def initChannels(a: Int, b: Int, c: Option[Int] = None) =
    c1.clear()
    c2.clear()
    c3.clear()
    c1.delayedSend(a)
    c2.delayedSend(b)
    c.map(c3.delayedSend(_))

  def readCats[A](ch: Channel[A]) = cats.effect.IO.delay(ch.delayedRead())
  def sendCats[A](ch: Channel[A], a: A) = cats.effect.IO.delay(ch.delayedSend(a))
  def readZIO[A](ch: Channel[A]) = ZIO.attempt(ch.read())
  def sendZIO[A](ch: Channel[A], a: A) = ZIO.attempt(ch.send(a))

  test("applicatives"):
    initChannels(7, 8, Some(9))

    val resCats2 =
      for
        a <- readCats(c1)
        b <- readCats(c2)
      yield a * b

    val resCats3 =
      for
        a <- readCats(c1)
        b <- readCats(c2)
        c <- readCats(c3)
      yield a + b + c

    val resZio2 =
      for
        a <- readZIO(c1)
        b <- readZIO(c2)
      yield a * b

    val resZio3 =
      for
        a <- readZIO(c1)
        b <- readZIO(c2)
        c <- readZIO(c3)
      yield a * b * c

    val resA2 = Some(c1.delayedRead()) * Some(c2.delayedRead())
    val resA3 = Future(c1.delayedRead()) + Future(c2.delayedRead()) + Future(c3.delayedRead())

    val resB2 = Try(c1.delayedRead()) * Try(c2.delayedRead())
    val resB3 = Future(c1.delayedRead()) + Future(c2.delayedRead()) + Future(c3.delayedRead())

    val resC2 = Right(c1.delayedRead()) * Right(c2.delayedRead())
    val resC3 = Future(c1.delayedRead()) * Future(c2.delayedRead()) * Future(c3.delayedRead())

    val resD2 = Try(c1.delayedRead()) * Try(c2.delayedRead())
    val resD3 = Some(c1.delayedRead()) * Some(c2.delayedRead()) * Some(c3.delayedRead())

    val resCats2Val = resCats2.value()
    val resCats3Val = resCats3.value()
    val resZio2Val = resZio2.value()
    val resZio3Val = resZio3.value()
    assertEquals(resA2.value(), resCats2Val)
    assertEquals(resA3.value(), resCats3Val)
    assertEquals(resB2.value(), resCats2Val)
    assertEquals(resB3.value(), resCats3Val)

    assertEquals(resC2.value(), resZio2Val)
    assertEquals(resC3.value(), resZio3Val)
    assertEquals(resD2.value(), resZio2Val)
    assertEquals(resD3.value(), resZio3Val)

  def getChannelIO[T[_]: Mappable](using Effects[T])() =
    val res1 =
      sequence(
        c1.read(),
        c2.read(),
        _ + _,
        c3.send
      )
    val res2 =
      sequence(
        c1.read(),
        c2.read(),
        (a, b) => c3.send(a + b)
      )
    (res1, res2)

  def catsChannel() =
    for
      a <- readCats(c1)
      b <- readCats(c2)
      _ <- sendCats(c3, a + b)
    yield ()

  def zioChannelA() =
    for
      a <- readZIO(c1)
      b <- readZIO(c2)
      _ <- sendZIO(c3, a + b)
    yield ()

  def zioChannelB() =
    defer {
      val a = c1.read()
      val b = c2.read()
      c3.send(a + b)
    }

  test("Basic IO channel"):
    /** by default: BasicIO */
    val (res1, res2) = getChannelIO()

    initChannels(3, 7)
    res1.value()
    val chRes1 = c3.read()
    initChannels(4, 6)
    res2.value()
    val chRes2 = c3.read()
    assertEquals(chRes1, chRes2)

  test("Cats channel"):
    given Effects[cats.effect.IO] = Effects()

    val (res1, res2): (cats.effect.IO[Unit], cats.effect.IO[Unit]) = getChannelIO()

    initChannels(1, 9)
    catsChannel().value()
    val chResCats = c3.read()
    initChannels(2, 8)
    res1.value()
    val chRes1 = c3.read()
    initChannels(3, 7)
    res2.value()
    val chRes2 = c3.read()
    assertEquals(chRes1, chRes2)
    assertEquals(chRes1, chResCats)

  test("ZIO channel"):
    given Effects[zIO] = Effects()

    val (res1, res2): (zIO[Unit], zIO[Unit]) = getChannelIO()

    initChannels(1, 9)
    zioChannelA().value()
    val chResZioA = c3.read()
    initChannels(2, 8)
    zioChannelB().value()
    val chResZioB = c3.read()
    initChannels(3, 7)
    res1.value()
    val chRes1 = c3.read()
    initChannels(4, 6)
    res2.value()
    val chRes2 = c3.read()
    assertEquals(chResZioA, chResZioB)
    assertEquals(chRes1, chRes2)
    assertEquals(chRes1, chResZioA)

  test("Cats mapN / parMapN"):
    val delay = 25
    def f(n: Int) =
      cats.effect.IO:
        Thread.sleep(delay)
        n * 10

    val io1 = f(3) + f(4) + f(5)
    val io2 = image(f(3), f(4), f(5), _ + _ + _)
    val io3 = (f(3), f(4), f(5)).mapN(_ + _ + _)
    val io4 = (f(3), f(4), f(5)).parMapN(_ + _ + _)

    val (res1, t1) = timed(io1.value())
    val (res2, t2) = timed(io2.value())
    val (res3, t3) = timed(io3.value())
    val (res4, t4) = timed(io4.value())
    val isParallel1 = t1 < 2 * delay * 1000
    val isParallel2 = t2 < 2 * delay * 1000
    val isParallelMapN = t3 < 2 * delay * 1000
    val isParallelParMapN = t4 < 2 * delay * 1000

    assertEquals(res1, res2)
    assertEquals(res2, res3)
    assertEquals(res3, res4)
    assertEquals(isParallel1, true)
    assertEquals(isParallel2, true)
    assertEquals(isParallelMapN, false)
    assertEquals(isParallelParMapN, true)

  test("Cats / ZIO parallel"):
    val n = 3
    def delayedSendToChannel(s: String, n: Int) =
      for (i <- 1 to n)
        ch.delayedSend(s"$s$i", 25)

    val ioCatsA = cats.effect.IO(delayedSendToChannel("A", n))
    val ioCatsB = cats.effect.IO(delayedSendToChannel("B", n))
    val ioCatsC = cats.effect.IO(delayedSendToChannel("C", n))

    val zioA = ZIO.attempt(delayedSendToChannel("A", n))
    val zioB = ZIO.attempt(delayedSendToChannel("B", n))
    val zioC = ZIO.attempt(delayedSendToChannel("C", n))

    val ioBasicA = BasicIO(() => delayedSendToChannel("A", n))
    val ioBasicB = BasicIO(() => delayedSendToChannel("B", n))
    val ioBasicC = BasicIO(() => delayedSendToChannel("C", n))

    /** Cats: sequential */
    val catsSeqIO =
      (ioCatsA, ioCatsB, ioCatsC).mapN:
        (_, _, _)

    /** Cats: parallel */
    val catsParIO =
      (ioCatsA, ioCatsB, ioCatsC).parMapN:
        (_, _, _)

    /** ZIO: sequential */
    val zioSeqIO = zioA.zip(zioB.zip(zioC))

    /** ZIO: parallel */
    val zioParIO = zioA.zipPar(zioB.zipPar(zioC))

    /** Basic IO (parallel) */
    val basicParCats = image(ioCatsA, ioCatsB, ioCatsC, (_, _, _))
    val basicParZIO = image(zioA, zioB, zioC, (_, _, _))
    val basicPar = image(ioBasicA, ioBasicB, ioBasicC, (_, _, _))

    val seqContents = List("A1", "A2", "A3")
    val parContents = Set("A1", "B1", "C1")

    ch.clear()
    catsSeqIO.value()
    assertEquals(ch.contents.take(3), seqContents)

    ch.clear()
    zioSeqIO.value()
    assertEquals(ch.contents.take(3), seqContents)

    ch.clear()
    catsParIO.value()
    assertEquals(ch.contents.take(3).toSet, parContents)

    ch.clear()
    zioParIO.value()
    assertEquals(ch.contents.take(3).toSet, parContents)

    ch.clear()
    basicParCats.value()
    assertEquals(ch.contents.take(3).toSet, parContents)

    ch.clear()
    basicParZIO.value()
    assertEquals(ch.contents.take(3).toSet, parContents)

    ch.clear()
    basicPar.value()
    assertEquals(ch.contents.take(3).toSet, parContents)
