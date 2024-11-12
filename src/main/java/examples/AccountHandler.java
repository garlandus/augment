package examples;

import static augmented.augmentJ.*;
import util.JavaUtil.Pair;
import scala.runtime.*;

import zio.Ref;
import zio.ZIO;

/** Adapted from https://www.learnscala.dev/challenge-page/zio-2-functional-programming-fundamentals-course */

public class AccountHandler
{
  public static class Account {
    public Double balance;

    Account(Double bal) {
      this.balance = bal;
    }
  }

  public static ZIO<Object, Nothing$, Double> withdraw(Ref<Account> account, Double amount) {
    return zioRefModify(account, acc -> {
      if (acc.balance >= amount) {
        var newBalance = acc.balance - amount;
        return new Pair<Double, Account>(amount, new Account(newBalance));
      } else
        return new Pair<Double, Account>(0.0, acc);
    });
  }

  public static ZIO<Object, Object, BoxedUnit> getAccountIO() {
    var prog = 
      sequence(
        ()                    -> zioRefMake(new Account (100.0)),
        account               -> withdraw(account, 70.0),
        (account, _)          -> withdraw(account, 50.0),
        (account, _, _)       -> zioRefGet(account),
        (withdrawal1, _, _)   -> zioPrintLine("Withdrawal 1: $" + withdrawal1),
        (withdrawal2, _, _)   -> zioPrintLine("Withdrawal 2: $" + withdrawal2),
        (finalBalance, _, _)  -> zioPrintLine("Final balance: $" + finalBalance.balance)
      );

    return prog.io();
  }
}
