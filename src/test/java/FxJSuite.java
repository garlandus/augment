import static augmented.augmentJ.*;
import static mappable.Mapper.*;
import static multiarray.multiArray.*;
import static util.JavaUtil.*;
import multiarray.ArrayB;
import multiarray.MultiArrayB;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.concurrent.*;
import java.util.function.*;
import java.util.List;
import java.util.Optional;
import java.util.Random;
import java.util.stream.*;

import scala.util.Either;
import scala.util.Try;

import org.junit.Test;
import static org.junit.Assert.*;

public class FxJSuite {

  @Test
  public void addAndMultOptions() {
    var mult = augment((Integer a, Integer b) -> a * b);
    var xOpt = mappable(4, a -> Optional.of(a));

    var res0 = mult.apply(4, 5);
    var res1 = mult.apply(xOpt, 5);
    var res2 = mult.apply(5, xOpt);

    assertEquals(res0, (Integer) 20);
    assertEquals(res1.mappable(), Optional.of(20));
    assertEquals(res1.hasValue(), true);
    assertEquals(res1.value(), (Integer) 20);
    assertEquals(res2, res1);
  }

  @Test
  public void propagateOrBubbleUp() {
    var mult = augment((Integer a, Integer b) -> a * b);
    var add = augment((Integer a, Integer b, Integer c) -> a + b + c);

    var xOpt = mappable(4, a -> Optional.of(a));
    var x0 = mult.apply(4, 5);
    var y0 = add.apply(2, x0, 3);
    var z0 = mult.apply(4, y0);
    assertEquals(z0, (Integer) 100);

    var x = mult.apply(xOpt, 5);
    var y = add.apply(2, x, 3);
    var z = mult.apply(4, y);
    assertEquals(z.mappable(), Optional.of(100));
    assertEquals(z.hasValue(), true);
    assertEquals(z.value(), (Integer) 100);
  }

  @Test
  public void propagateEither() {
    var mult = augment((Integer a, Integer b) -> a * b);
    var add = augment((Integer a, Integer b, Integer c) -> a + b + c);

    var xDerived = mappable(4, a -> new scala.util.Left("not found"));
    var x = mult.apply(xDerived, 5);
    var y = add.apply(2, x, 3);
    var z = mult.apply(4, y);

    assertEquals(z.mappable() instanceof Either, true);
    assertEquals(z.mappable(), new scala.util.Left("not found"));
    assertEquals(z.hasValue(), false);
  }

  @Test
  public void propagateTry() {
        var mult = augment((Integer a, Integer b) -> a * b);
    var add = augment((Integer a, Integer b, Integer c) -> a + b + c);

    var derivedVal1 = mappable(4, a -> Try.apply(() -> 1 / 0));
    var x1 = mult.apply(derivedVal1, 5);
    var y1 = add.apply(2, x1, 3);
    var z1 = mult.apply(4, y1);

    assertEquals(z1.mappable() instanceof Try, true);
    assertEquals(((Try<Integer>) z1.mappable()).fold(x -> x.toString(), y -> ""),
      "java.lang.ArithmeticException: / by zero");
    assertEquals(z1.hasValue(), false);

    var derivedVal = mappable(4, a -> Try.apply(() -> a));
    var x = mult.apply(derivedVal, 5);
    var y = add.apply(2, x, 3);
    var z = mult.apply(4, y);

    assertEquals(z.mappable() instanceof Try, true);
    assertEquals(z.mappable(), new scala.util.Success(100));
    assertEquals(z.hasValue(), true);
    assertEquals(z.value(), (Integer) 100);
  }

  public class RandomWait {
    private ExecutorService executor = Executors.newSingleThreadExecutor();
    Random rand = new Random();

    public Future<Integer> start(Integer i) {
      var msWaited = 10 + rand.nextInt(100);
      return executor.submit(() -> {
        println("Waiting " + msWaited + " ms...");
        Thread.sleep(msWaited);
        return i;
      });
    }
  }

  @Test
  public void propagateFuture() throws InterruptedException {
    var mult = augment((Integer a, Integer b) -> a * b);
    var add = augment((Integer a, Integer b, Integer c) -> a + b + c);

    var derivedVal = mappable(4, a -> new RandomWait().start(a));

    var x = mult.apply(derivedVal, 5);
    var y = add.apply(2, x, 3);
    var z = mult.apply(4, y);

    assertEquals(z.mappable() instanceof FutureTask, true);
    assertEquals(z.hasValue(), false);
    Thread.sleep(150);
    assertEquals(z.hasValue(), true);
    assertEquals(z.value(), (Integer) 100);
  }

  public static String readLineStub() {
    return "100";
  }

  @Test
  public void basicIO() {
    var r = sequence(
      () -> println("\n\nEnter a number:"),
      () -> readLineStub(),
      s -> Integer.parseInt(s),
      n -> {
        println("Number entered: " + n);
        return n;
      });
    assertEquals(r.value(), (Integer) 100);
  }

  public static void println() {
    System.out.println();
  }

  public static void println(String s) {
    System.out.println(s);
  }

  public static <A> void println(A a) {
    System.out.println(a.toString());
  }
}
