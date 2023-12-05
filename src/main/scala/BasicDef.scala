package basicdef

type JInteger = java.lang.Integer
type JDouble = java.lang.Double
type JList[A] = java.util.List[A]
type JMap[A, B] = java.util.Map[A, B]
type JStream[A] = java.util.stream.Stream[A]
type JFunction[A, B] = java.util.function.Function[A, B]
type JDepSeq[A, B] = A => JList[B]
type JDepStream[A, B] = A => java.util.stream.BaseStream[B, _]

def defaultMsg[A](a: A) = s"Condition not met by: $a"

case class Condition[A](condition: A => Boolean, msg: A => String = defaultMsg)
def EmptyCondition[A]() = Condition[A](_ => true, _ => "")

case class Conditions[Z, A](
    preCond: Condition[A],
    postCond: Condition[(Z, A)] = EmptyCondition[(Z, A)]()
):
  def exec(f: A => Z, a: A) =
    if !preCond.condition(a) then throw new Exception(preCond.msg(a))
    else
      val z = f(a)
      if !postCond.condition(z, a) then throw new Exception(postCond.msg(z, a))
      else z

case class Named[A](mainValue: A, name: String):
  override def toString(): String = name

opaque type URL = String
object URL:
  def apply(s: String): URL = s

type Thunk[A] = () => A

case class Logged[A](mainValue: A, log: String)

case class BasicIO[A](val thunk: () => A):
  def attempt: BasicIO[Either[Throwable, A]] =
    BasicIO(() =>
      try Right(thunk())
      catch case t: Throwable => Left(t)
    )
