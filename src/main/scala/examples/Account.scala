package examples

import augmented._
import mappable.given
import mappablethirdparty.given

import zio._
import zio.Console._

/** Adapted from https://www.learnscala.dev/challenge-page/zio-2-functional-programming-fundamentals-course */

type Money = Double
case class Account(balance: Money)

def withdraw(account: Ref[Account], amount: Money): ZIO[Any, Nothing, Money] =
  account.modify: acc =>
    if acc.balance >= amount then
      val newBalance = acc.balance - amount
      (amount, Account(newBalance))
    else (0.0, acc)

def getAccountIO(useComprehensions: Boolean = false) =

  /** 
    * Standard for-comprehension
    */
  val prog1 =
    for
      account       <- Ref.make(Account(100.0))
      withdrawal1   <- withdraw(account, 70.0)
      withdrawal2   <- withdraw(account, 50.0)
      finalBalance  <- account.get
      _             <- printLine(s"Withdrawal 1: $$$withdrawal1")
      _             <- printLine(s"Withdrawal 2: $$$withdrawal2")
      _             <- printLine(s"Final balance: $$${finalBalance.balance}")
    yield ()

  /** 
    * Unlike the previous version, here printLine is optional
    * (trying to replace printLine with println above will result in "value flatMap is not a member of Unit")
    */
  val prog2 =
    sequence(
      Ref.make(Account (100.0)),
      withdraw(_, 70.0),
      (account, _)          => withdraw(account, 50.0),
      (account, _, _)       => account.get,
      (withdrawal1, _, _)   => printLine(s"Withdrawal 1: $$$withdrawal1"),
      (withdrawal2, _, _)   => println(s"Withdrawal 2: $$$withdrawal2"),    /** printLine is not required */
      (finalBalance, _, _)  => println(s"Final balance: $$${finalBalance.balance}")
    )

  if useComprehensions then prog1 else prog2

