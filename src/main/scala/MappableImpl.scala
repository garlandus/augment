package mappable

import basicdef._
import shape._

import scala.concurrent._
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.reflect.ClassTag
import scala.util._

import collection.JavaConverters._
import java.util.concurrent.{ExecutorService, Executors, TimeUnit};

given Applicative[Option] with
  override def isDelayed: Boolean = false
  extension [A](a: => A) def unit(): Option[A] = Some(a)
  extension [A](as: Option[A])
    def hasValue(): Boolean = as.isDefined
    def value(): A = as.get
    def result(): Either[Throwable, A] =
      if as.hasValue() then Right(as.value()) else Left(Exception("No option value"))
    def map[B](f: A => B): Option[B] = as.map(f)
    def flatMap[B](f: A => Option[B]): Option[B] = as.flatMap(f)

  override def product[A, B](as: Option[A], bs: Option[B]): Option[(A, B)] =
    (as, bs) match
      case (Some(a), Some(b)) => Some((a, b))
      case _                  => None

given Applicative[Some] with
  override def isDelayed: Boolean = false
  extension [A](a: => A) def unit(): Some[A] = Some(a)
  extension [A](as: Some[A])
    def hasValue(): Boolean = true
    def value(): A = as.get
    def result(): Either[Throwable, A] = Right(value())
    def map[B](f: A => B): Some[B] = as.map(f).asInstanceOf[Some[B]]
    def flatMap[B](f: A => Some[B]): Some[B] = as.flatMap(f).asInstanceOf[Some[B]]

  override def product[A, B](as: Some[A], bs: Some[B]): Some[(A, B)] =
    (as, bs) match
      case (Some(a), Some(b)) => Some((a, b))

given [E]: Applicative[EitherX[E]] with
  override def isDelayed: Boolean = false
  extension [A](a: => A) def unit(): Either[E, A] = Right(a)
  extension [A](as: Either[E, A])
    def hasValue(): Boolean = as.isRight
    def value(): A =
      as match
        case Right(a) => a
        case Left(e)  => throw Exception(s"$e")
    def result(): Either[Throwable, A] = ???
    def map[B](f: A => B): Either[E, B] = as.map(f)
    def flatMap[B](f: A => Either[E, B]): Either[E, B] = as.flatMap(f)

  override def product[B1, B2](bs1: EitherX[E][B1], bs2: EitherX[E][B2]): EitherX[E][(B1, B2)] =
    (bs1, bs2) match
      case (Right(a), Right(b)) => Right((a, b))
      case (Left(l), _)         => Left(l)
      case (_, Left(l))         => Left(l)

given [E]: Applicative[RightX[E]] with
  override def isDelayed: Boolean = false
  extension [A](a: => A) def unit(): Right[E, A] = Right(a)
  extension [A](as: Right[E, A])
    def hasValue(): Boolean = true
    def value(): A = as.value
    def result(): Either[Throwable, A] = ???
    def map[B](f: A => B): Right[E, B] = as.map(f).asInstanceOf[Right[E, B]]
    def flatMap[B](f: A => Right[E, B]): Right[E, B] = as.flatMap(f).asInstanceOf[Right[E, B]]

  override def product[B1, B2](bs1: Right[E, B1], bs2: Right[E, B2]): Right[E, (B1, B2)] =
    (bs1, bs2) match
      case (Right(a), Right(b)) => Right((a, b))

given Applicative[Try] with
  override def isDelayed: Boolean = false
  extension [A](a: => A) def unit(): Try[A] = Try(a)
  extension [A](as: Try[A])
    def hasValue(): Boolean = as.isInstanceOf[Success[A]]
    def value(): A = as.getOrElse(throw new Exception("not valued"))
    def result(): Either[Throwable, A] =
      as.toEither
    def map[B](f: A => B): Try[B] = as.map(f)
    def flatMap[B](f: A => Try[B]): Try[B] = as.flatMap(f)

  override def product[A, B](as: Try[A], bs: Try[B]): Try[(A, B)] =
    (as, bs) match
      case (Success(a), Success(b)) => Success((a, b))
      case (Failure(e), _)          => Failure(e)
      case (_, Failure(e))          => Failure(e)

given Applicative[Success] with
  override def isDelayed: Boolean = false
  extension [A](a: => A) def unit(): Success[A] = Success(a)
  extension [A](as: Success[A])
    def hasValue(): Boolean = true
    def value(): A = as.getOrElse(throw new Exception("not valued"))
    def result(): Either[Throwable, A] = ???
    def map[B](f: A => B): Success[B] = as.map(f).asInstanceOf[Success[B]]
    def flatMap[B](f: A => Success[B]): Success[B] = as.flatMap(f).asInstanceOf[Success[B]]

  override def product[A, B](as: Success[A], bs: Success[B]): Success[(A, B)] =
    (as, bs) match
      case (Success(a), Success(b)) => Success((a, b))

given Mappable[Logged] with
  override def isDelayed: Boolean = false
  extension [A](a: => A) def unit(): Logged[A] = Logged(a, "")
  extension [A](as: Logged[A])
    def hasValue(): Boolean = true
    def value(): A = as.mainValue
    def result(): Either[Throwable, A] = ???
    def map[B](f: A => B): Logged[B] = Logged(f(as.mainValue), f"${as.log}")
    def flatMap[B](f: A => Logged[B]): Logged[B] =
      val x = f(as.mainValue)
      Logged(x.mainValue, f"${x.log} | ${as.log}")

  override def product[A, B](as: Logged[A], bs: Logged[B]): Logged[(A, B)] =
    (as, bs) match
      case (Logged(a, s1), Logged(b, s2)) => Logged((a, b), s1 + ", " + s2)

given Applicative[scala.concurrent.Future] with
  override def isDelayed: Boolean = false
  extension [A](a: => A) def unit(): scala.concurrent.Future[A] = scala.concurrent.Future(a)
  extension [A](as: scala.concurrent.Future[A])
    def hasValue(): Boolean = as.isCompleted
    def value(): A = Await.result(as, Duration.Inf)
    def map[B](f: A => B): scala.concurrent.Future[B] = as.map(f)
    def flatMap[B](f: A => scala.concurrent.Future[B]): scala.concurrent.Future[B] = as.flatMap(f)

  override def product[A, B](as: Future[A], bs: Future[B]): Future[(A, B)] =
    Future.sequence(List(as, bs)).map(l => (l(0).asInstanceOf[A], l(1).asInstanceOf[B]))

  override def product[A, B, C](as: Future[A], bs: Future[B], cs: Future[C]): Future[(A, B, C)] =
    Future.sequence(List(as, bs, cs)).map(l => (l(0).asInstanceOf[A], l(1).asInstanceOf[B], l(2).asInstanceOf[C]))

given JFuture: Applicative[java.util.concurrent.Future] with

  def executor: ExecutorService = Executors.newSingleThreadExecutor()
  override def isDelayed: Boolean = false
  extension [A](a: => A) def unit(): java.util.concurrent.Future[A] = executor.submit(() => a)
  extension [A](as: java.util.concurrent.Future[A])
    def hasValue(): Boolean = as.isDone()
    def value(): A = as.get(60, TimeUnit.SECONDS)
    def result(): Either[Throwable, A] = ???
    def map[B](f: A => B): java.util.concurrent.Future[B] =
      executor.submit(() => f(as.get()))
    def flatMap[B](f: A => java.util.concurrent.Future[B]): java.util.concurrent.Future[B] =
      executor.submit(() => f(as.get()).get())

  override def product[A, B](
      as: java.util.concurrent.Future[A],
      bs: java.util.concurrent.Future[B]
  ): java.util.concurrent.Future[(A, B)] =
    as.flatMap(a => bs.map(b => (a, b)))

given Applicative[java.util.Optional] with
  override def isDelayed: Boolean = false
  extension [A](a: => A) def unit(): java.util.Optional[A] = java.util.Optional.of(a)
  extension [A](as: java.util.Optional[A])
    def hasValue(): Boolean = as.isPresent()
    def value(): A = as.get
    def result(): Either[Throwable, A] = ???
    def map[B](f: A => B): java.util.Optional[B] =
      if (as.isEmpty) then java.util.Optional.empty() else java.util.Optional.of(f(as.get))
    def flatMap[B](f: A => java.util.Optional[B]): java.util.Optional[B] =
      if (as.isEmpty) then java.util.Optional.empty() else f(as.get)

  override def product[A, B](as: java.util.Optional[A], bs: java.util.Optional[B]): java.util.Optional[(A, B)] =
    if (as.isPresent() && bs.isPresent()) then java.util.Optional.of(as.get, bs.get) else java.util.Optional.empty()

given Mappable[scala.collection.immutable.List] with
  override def isDelayed: Boolean = false
  extension [A](a: => A) def unit(): List[A] = List(a)
  extension [A](as: List[A])
    def hasValue(): Boolean = !as.isEmpty
    def value(): A = ???
    def result(): Either[Throwable, A] = ???
    def map[B](f: A => B): List[B] = as.map(f)
    def flatMap[B](f: A => List[B]): List[B] = as.flatMap(f)
  override def product[A, B](as: List[A], bs: List[B]): List[(A, B)] =
    as.zip(bs)

given JList: Mappable[java.util.List] with
  override def isDelayed: Boolean = false
  extension [A](a: => A) def unit(): java.util.List[A] = java.util.List.of(a)
  extension [A](as: java.util.List[A])
    def hasValue(): Boolean = !as.isEmpty
    def value(): A = ???
    def result(): Either[Throwable, A] = ???
    def map[B](f: A => B): java.util.List[B] =
      as.asScala.toList.map(f).asJava
    def flatMap[B](f: A => java.util.List[B]): java.util.List[B] =
      as.asScala.flatMap(f(_).asScala).asJava

given Mappable[BasicIO] with
  extension [A](a: => A) def unit(): BasicIO[A] = BasicIO(() => a)
  extension [A](as: BasicIO[A])
    def hasValue(): Boolean = true
    def value(): A = as.thunk()
    def result(): Either[Throwable, A] = ???
    def map[B](f: A => B): BasicIO[B] =
      BasicIO(() => f(as.thunk()))
    def flatMap[B](f: A => BasicIO[B]): BasicIO[B] =
      BasicIO(() => f(as.thunk()).thunk())

  override def product[A, B](as: BasicIO[A], bs: BasicIO[B]): BasicIO[(A, B)] =
    BasicIO(() =>
      val m = summon[Applicative[Future]]
      val fut = m.product(Future(as.thunk()), Future(bs.thunk()))
      fut.value()
    )
  override def product[A, B, C](as: BasicIO[A], bs: BasicIO[B], cs: BasicIO[C]): BasicIO[(A, B, C)] =
    BasicIO(() =>
      val m = summon[Applicative[Future]]
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

given Applicative[Thunk] with
  extension [A](a: => A) def unit(): Thunk[A] = () => a
  extension [A](as: Thunk[A])
    def hasValue(): Boolean = true
    def value(): A = as()
    def result(): Either[Throwable, A] = ???
    def map[B](f: A => B): Thunk[B] = () => f(as())
    def flatMap[B](f: A => Thunk[B]): Thunk[B] = () => f(as())()

given Mappable[scala.collection.immutable.Vector] with
  override def isDelayed: Boolean = false
  extension [A](a: => A) def unit(): Vector[A] = Vector(a)
  extension [A](as: Vector[A])
    def hasValue(): Boolean = !as.isEmpty
    def value(): A = ???
    def result(): Either[Throwable, A] = ???
    def map[B](f: A => B): Vector[B] = as.map(f)
    def flatMap[B](f: A => Vector[B]): Vector[B] = as.flatMap(f)
  override def product[A, B](as: Vector[A], bs: Vector[B]): Vector[(A, B)] =
    as.zip(bs)

given Mappable[IndexedSeq] with
  override def isDelayed: Boolean = false
  extension [A](a: => A) def unit(): IndexedSeq[A] = IndexedSeq(a)
  extension [A](as: IndexedSeq[A])
    def hasValue(): Boolean = !as.isEmpty
    def value(): A = ???
    def result(): Either[Throwable, A] = ???
    def map[B](f: A => B): IndexedSeq[B] = as.map(f)
    def flatMap[B](f: A => IndexedSeq[B]): IndexedSeq[B] = as.flatMap(f)
  override def product[A, B](as: IndexedSeq[A], bs: IndexedSeq[B]): IndexedSeq[(A, B)] =
    as.zip(bs)

given [A]: Plain[A] with
  override def isPlain = true

given [T[_]: Mappable, A]: Plain[T[A]] with
  override def isPlain = false

given [F[_]]: ContainerPair[F, F] with
  def tag = "SAME"
  def areSame(): Boolean = true

given [F[_], G[_]]: ContainerPair[F, G] with
  def tag = "DIFF"
  def areSame(): Boolean = false

given [A]: Atomic[A] with
  def tag = "A"

given Atomic[Unit] with
  def tag = "Unit"
given Atomic[Boolean] with
  def tag = "Boolean"
given Atomic[Char] with
  def tag = "Char"
given Atomic[String] with
  def tag = "String"
given Atomic[Int] with
  def tag = "Int"
given Atomic[Float] with
  def tag = "Float"
given Atomic[Double] with
  def tag = "Double"  

given [T[_]: Applicative, U[_]: Applicative]: ContainerTriple[[A] =>> T[U[A]], T, U] with
  def tag = "TU"
given [T[_]: Applicative, U[_]: Applicative]: ContainerTriple[[A] =>> T[U[A]], U, T] with
  def tag = "UT"

given Eitherish[Either] with
  val tag = "Either"
  extension [E, A](e: E) def toLeft() = Left(e)
  extension [E, A](a: A) def toRight() = Right(a)
  extension [E, A](x: Either[E, A]) def asEither()(using ClassTag[A]) = x
  extension [E, A](x: Either[E, A]) def fromEither() = x

given Eitherish[Right] with
  val tag = "Right"
  extension [E, A](e: E) def toLeft() = ???
  extension [E, A](a: A) def toRight() = Right(a)
  extension [E, A](x: Right[E, A]) def asEither()(using ClassTag[A]) = x
  extension [E, A](x: Either[E, A]) def fromEither(): Right[E, A] = ???

given Eitherish[OptionE] with
  val tag = "OptionE"
  extension [E, A](e: E) def toLeft() = None
  extension [E, A](a: A) def toRight() = Some(a)
  extension [E, A](x: OptionE[E, A])
    def asEither()(using ClassTag[A]) =
      x match
        case Some(a) => Right(a)
        case None    => Left(Exception("N/A").asInstanceOf[E])
  extension [E, A](x: Either[E, A]) def fromEither() = x.toOption

given Eitherish[TryE] with
  val tag = "TryE"
  extension [E, A](e: E) def toLeft() = Failure(e.asInstanceOf[Throwable])
  extension [E, A](a: A) def toRight() = Success(a)
  extension [E, A](x: TryE[E, A])
    def asEither()(using ClassTag[A]): Either[E, A] =
      x match
        case Success(a: A) => Right(a)
        case Failure(e)    => Left(e.asInstanceOf[E])
  extension [E, A](x: Either[E, A])
    def fromEither() =
      x match
        case Right(a) => Success(a)
        case Left(e) =>
          e match
            case t: Throwable => Failure(t)
            case _            => Failure(Exception(f"$e"))

given EitherishPair[OptionE, OptionE] with
  val tag = "OptionE / OptionE"
  extension [E, A](x: OptionE[E, A])
    def asEitherP()(using ClassTag[A]) =
      x match
        case Some(a) => Right(a)
        case None    => Left(Exception("N/A").asInstanceOf[E])
  extension [E, A](x: Either[E, A]) def fromEitherP() = x.toOption

given EitherishPair[Either, Either] with
  val tag = "Either / Either"
  extension [E, A](x: Either[E, A]) def asEitherP()(using ClassTag[A]) = x
  extension [E, A](x: Either[E, A]) def fromEitherP() = x

given EitherishPair[Right, Either] with
  val tag = "Right / Either"
  extension [E, A](x: Right[E, A]) def asEitherP()(using ClassTag[A]) = x
  extension [E, A](x: Either[E, A]) def fromEitherP(): Either[E, A] = x

given EitherishPair[TryE, TryE] with
  val tag = "TryE / TryE"
  extension [E, A](x: TryE[E, A])
    def asEitherP()(using ClassTag[A]): Either[E, A] =
      x match
        case Success(a: A) => Right(a)
        case Failure(e)    => Left(e.asInstanceOf[E])
  extension [E, A](x: Either[E, A])
    def fromEitherP() =
      x match
        case Right(a) => Success(a)
        case Left(e) =>
          e match
            case t: Throwable => Failure(t)
            case _            => Failure(Exception(f"$e"))

given EitherishOne[Option, OptionE] with
  val tag = "Option / OptionE"
  extension [A](as: Option[A]) def convertToEitherish() = as

given EitherishOne[Try, TryE] with
  val tag = "Try / TryE"
  extension [A](as: Try[A]) def convertToEitherish() = as

object Mapper:

  def mappable[A](a: A, unit: A => Any): MappableW[A, ?] =
    val x = unit(a)
    x match
      case (x: java.util.Optional[_])          => MappableW(x.asInstanceOf[java.util.Optional[A]])
      case (x: java.util.concurrent.Future[_]) => MappableW(x.asInstanceOf[java.util.concurrent.Future[A]])
      case (x: Try[_])                         => MappableW(x.asInstanceOf[Try[A]])
      case (x: Either[_, _])                   => MappableW(x.asInstanceOf[Either[?, A]])
      case _                                   => ???
