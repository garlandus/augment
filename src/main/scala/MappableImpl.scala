package mappable

import basicdef._
import shape._

import scala.concurrent._
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.reflect.ClassTag
import scala.util._

import java.util.concurrent.{ExecutorService, Executors, TimeUnit};

given Mappable[Option] with
  override def isDelayed: Boolean = false
  extension [A](as: Option[A]) def hasValue(): Boolean = as.isDefined
  extension [A](as: Option[A]) def value(): A = as.get
  extension [A](a: => A) def unit(): Option[A] = Some(a)
  extension [A](as: Option[A]) def map[B](f: A => B): Option[B] = as.map(f)
  extension [A](as: Option[A]) def flatMap[B](f: A => Option[B]): Option[B] = as.flatMap(f)

  override def product[A, B](as: Option[A], bs: Option[B]): Option[(A, B)] =
    (as, bs) match
      case (Some(a), Some(b)) => Some((a, b))
      case _                  => None

given Mappable[Some] with
  override def isDelayed: Boolean = false
  extension [A](as: Some[A]) def hasValue(): Boolean = true
  extension [A](as: Some[A]) def value(): A = as.get
  extension [A](a: => A) def unit(): Some[A] = Some(a)
  extension [A](as: Some[A]) def map[B](f: A => B): Some[B] = as.map(f).asInstanceOf[Some[B]]
  extension [A](as: Some[A]) def flatMap[B](f: A => Some[B]): Some[B] = as.flatMap(f).asInstanceOf[Some[B]]

  override def product[A, B](as: Some[A], bs: Some[B]): Some[(A, B)] =
    (as, bs) match
      case (Some(a), Some(b)) => Some((a, b))

type EitherX = [A] =>> [B] =>> Either[A, B]

given [A]: Mappable[EitherX[A]] with
  override def isDelayed: Boolean = false
  extension [B](bs: Either[A, B])
    def hasValue(): Boolean =
      bs match
        case Right(b) => true
        case Left(a)  => false

  extension [B](bs: Either[A, B])
    def value(): B =
      bs match
        case Right(b) => b
        case Left(a)  => throw Exception(s"$a")

  extension [B](b: => B) def unit(): Either[A, B] = Right(b)
  extension [B1](bs: Either[A, B1]) def map[B2](f: B1 => B2): Either[A, B2] = bs.map(f)
  extension [B1](bs: Either[A, B1]) def flatMap[B2](f: B1 => Either[A, B2]): Either[A, B2] = bs.flatMap(f)

  override def product[B1, B2](bs1: EitherX[A][B1], bs2: EitherX[A][B2]): EitherX[A][(B1, B2)] =
    (bs1, bs2) match
      case (Right(a), Right(b)) => Right((a, b))
      case (Left(l), _)         => Left(l)
      case (_, Left(l))         => Left(l)

type RightX = [A] =>> [B] =>> Right[A, B]
given [A]: Mappable[RightX[A]] with
  override def isDelayed: Boolean = false
  extension [B](as: Right[A, B]) def hasValue(): Boolean = true
  extension [B](bs: Right[A, B]) def value(): B = bs.value
  extension [B](b: => B) def unit(): Right[A, B] = Right(b)
  extension [B1](bs: Right[A, B1]) def map[B2](f: B1 => B2): Right[A, B2] = bs.map(f).asInstanceOf[Right[A, B2]]
  extension [B1](bs: Right[A, B1])
    def flatMap[B2](f: B1 => Right[A, B2]): Right[A, B2] = bs.flatMap(f).asInstanceOf[Right[A, B2]]

  override def product[B1, B2](bs1: Right[A, B1], bs2: Right[A, B2]): Right[A, (B1, B2)] =
    (bs1, bs2) match
      case (Right(a), Right(b)) => Right((a, b))

given Mappable[Try] with
  override def isDelayed: Boolean = false
  extension [A](as: Try[A]) def hasValue(): Boolean = as.isInstanceOf[Success[A]]
  extension [A](as: Try[A]) def value(): A = as.getOrElse(throw new Exception("not valued"))
  extension [A](a: => A) def unit(): Try[A] = Try(a)
  extension [A](as: Try[A]) def map[B](f: A => B): Try[B] = as.map(f)
  extension [A](as: Try[A]) def flatMap[B](f: A => Try[B]): Try[B] = as.flatMap(f)

  override def product[A, B](as: Try[A], bs: Try[B]): Try[(A, B)] =
    (as, bs) match
      case (Success(a), Success(b)) => Success((a, b))
      case (Failure(e), _)          => Failure(e)
      case (_, Failure(e))          => Failure(e)

given Mappable[Success] with
  override def isDelayed: Boolean = false
  extension [A](as: Success[A]) def hasValue(): Boolean = true
  extension [A](as: Success[A]) def value(): A = as.getOrElse(throw new Exception("not valued"))
  extension [A](a: => A) def unit(): Success[A] = Success(a)
  extension [A](as: Success[A]) def map[B](f: A => B): Success[B] = as.map(f).asInstanceOf[Success[B]]
  extension [A](as: Success[A]) def flatMap[B](f: A => Success[B]): Success[B] = as.flatMap(f).asInstanceOf[Success[B]]

  override def product[A, B](as: Success[A], bs: Success[B]): Success[(A, B)] =
    (as, bs) match
      case (Success(a), Success(b)) => Success((a, b))

given Mappable[Logged] with
  override def isDelayed: Boolean = false
  extension [A](as: Logged[A]) def hasValue(): Boolean = true
  extension [A](as: Logged[A]) def value(): A = as.mainValue
  extension [A](a: => A) def unit(): Logged[A] = Logged(a, "")
  extension [A](as: Logged[A]) def map[B](f: A => B): Logged[B] = Logged(f(as.mainValue), f"${as.log}")
  extension [A](as: Logged[A])
    def flatMap[B](f: A => Logged[B]): Logged[B] =
      val x = f(as.mainValue)
      Logged(x.mainValue, f"${x.log} | ${as.log}")

  override def product[A, B](as: Logged[A], bs: Logged[B]): Logged[(A, B)] =
    (as, bs) match
      case (Logged(a, s1), Logged(b, s2)) => Logged((a, b), s1 + ", " + s2)

given Mappable[scala.concurrent.Future] with
  override def isDelayed: Boolean = false
  extension [A](as: scala.concurrent.Future[A]) def hasValue(): Boolean = as.isCompleted
  extension [A](as: scala.concurrent.Future[A])
    def value(): A =
      Await.result(as, scala.concurrent.duration.Duration(60, SECONDS))
  extension [A](a: => A) def unit(): scala.concurrent.Future[A] = scala.concurrent.Future(a)
  extension [A](as: scala.concurrent.Future[A]) def map[B](f: A => B): scala.concurrent.Future[B] = as.map(f)
  extension [A](as: scala.concurrent.Future[A])
    def flatMap[B](f: A => scala.concurrent.Future[B]): scala.concurrent.Future[B] = as.flatMap(f)

  override def product[A, B](as: Future[A], bs: Future[B]): Future[(A, B)] =
    Future.sequence(List(as, bs)).map(l => (l(0).asInstanceOf[A], l(1).asInstanceOf[B]))

  override def product[A, B, C](as: Future[A], bs: Future[B], cs: Future[C]): Future[(A, B, C)] =
    Future.sequence(List(as, bs, cs)).map(l => (l(0).asInstanceOf[A], l(1).asInstanceOf[B], l(2).asInstanceOf[C]))

given JFuture: Mappable[java.util.concurrent.Future] with

  def executor: ExecutorService = Executors.newSingleThreadExecutor()
  override def isDelayed: Boolean = false
  extension [A](as: java.util.concurrent.Future[A]) def hasValue(): Boolean = as.isDone()
  extension [A](as: java.util.concurrent.Future[A]) def value(): A = as.get(60, TimeUnit.SECONDS)

  extension [A](a: => A) def unit(): java.util.concurrent.Future[A] = executor.submit(() => a)
  extension [A](as: java.util.concurrent.Future[A])
    def map[B](f: A => B): java.util.concurrent.Future[B] =
      executor.submit(() => f(as.get()))

  extension [A](as: java.util.concurrent.Future[A])
    def flatMap[B](f: A => java.util.concurrent.Future[B]): java.util.concurrent.Future[B] =
      executor.submit(() => f(as.get()).get())

  override def product[A, B](
      as: java.util.concurrent.Future[A],
      bs: java.util.concurrent.Future[B]
  ): java.util.concurrent.Future[(A, B)] =
    as.flatMap(a => bs.map(b => (a, b)))

given Mappable[java.util.Optional] with
  override def isDelayed: Boolean = false
  extension [A](as: java.util.Optional[A]) def hasValue(): Boolean = as.isPresent()
  extension [A](as: java.util.Optional[A]) def value(): A = as.get

  extension [A](a: => A) def unit(): java.util.Optional[A] = java.util.Optional.of(a)
  extension [A](as: java.util.Optional[A])
    def map[B](f: A => B): java.util.Optional[B] =
      if (as.isEmpty) then java.util.Optional.empty() else java.util.Optional.of(f(as.get))

  extension [A](as: java.util.Optional[A])
    def flatMap[B](f: A => java.util.Optional[B]): java.util.Optional[B] =
      if (as.isEmpty) then java.util.Optional.empty() else f(as.get)

  override def product[A, B](as: java.util.Optional[A], bs: java.util.Optional[B]): java.util.Optional[(A, B)] =
    if (as.isPresent() && bs.isPresent()) then java.util.Optional.of(as.get, bs.get) else java.util.Optional.empty()

given Mappable[List] with
  override def isDelayed: Boolean = false
  extension [A](as: List[A]) def hasValue(): Boolean = !as.isEmpty
  extension [A](as: List[A]) def value(): A = ???
  extension [A](a: => A) def unit(): List[A] = List(a)
  extension [A](as: List[A]) def map[B](f: A => B): List[B] = as.map(f)
  extension [A](as: List[A]) def flatMap[B](f: A => List[B]): List[B] = as.flatMap(f)

given Mappable[BasicIO] with
  extension [A](as: BasicIO[A]) def hasValue(): Boolean = true
  extension [A](as: BasicIO[A]) def value(): A = as.thunk()
  extension [A](a: => A) def unit(): BasicIO[A] = BasicIO(() => a)
  extension [A](as: BasicIO[A])
    def map[B](f: A => B): BasicIO[B] =
      BasicIO(() => f(as.thunk()))
  extension [A](as: BasicIO[A])
    def flatMap[B](f: A => BasicIO[B]): BasicIO[B] =
      BasicIO(() => f(as.thunk()).thunk())

  override def product[A, B](as: BasicIO[A], bs: BasicIO[B]): BasicIO[(A, B)] =
    BasicIO(() =>
      val m = summon[Mappable[Future]]
      val fut = m.product(Future(as.thunk()), Future(bs.thunk()))
      fut.value()
    )
  override def product[A, B, C](as: BasicIO[A], bs: BasicIO[B], cs: BasicIO[C]): BasicIO[(A, B, C)] =
    BasicIO(() =>
      val m = summon[Mappable[Future]]
      val fut = m.product(Future(as.thunk()), Future(bs.thunk()), Future(cs.thunk()))
      fut.value()
    )

  def retryGetValue[A](as: BasicIO[A], n: Int): A =
    Try(as.value()) match
      case Success(a) => a
      case Failure(e) => if (n > 0) then retryGetValue(as, n - 1) else throw Exception(e)
  extension [A](as: BasicIO[A])
    override def retry(n: Int): BasicIO[A] =
      BasicIO(() => retryGetValue(as, n))

given Mappable[Thunk] with
  extension [A](a: => A) def unit(): Thunk[A] = () => a
  extension [A](as: Thunk[A]) def hasValue(): Boolean = true
  extension [A](as: Thunk[A]) def value(): A = as()
  extension [A](as: Thunk[A]) def map[B](f: A => B): Thunk[B] = () => f(as())
  extension [A](as: Thunk[A]) def flatMap[B](f: A => Thunk[B]): Thunk[B] = () => f(as())()

object Mapper:

  def mappable[A](a: A, unit: A => Any): MappableW[A, _] =
    val x = unit(a)
    x match
      case (x: java.util.Optional[_])          => MappableW(x.asInstanceOf[java.util.Optional[A]])
      case (x: java.util.concurrent.Future[_]) => MappableW(x.asInstanceOf[java.util.concurrent.Future[A]])
      case (x: Try[_])                         => MappableW(x.asInstanceOf[Try[A]])
      case (x: Either[_, _])                   => MappableW(x.asInstanceOf[Either[_, A]])
      case _                                   => ???
