package examples;

import static util.ZioUtil.sequence;
import augmented.augmentJ.ZIO3;

public class RustArithmetic {
  /**
   * Converted from https://doc.rust-lang.org/rust-by-example/std/result/question_mark.html
   */

  public static class DivisionByZero extends Exception {
    public DivisionByZero(String msg) {
      super(msg);
    }
  }

  public static class NegativeSquareRoot extends Exception {
    public NegativeSquareRoot(String msg) {
      super(msg);
    }
  }

  public static class NonPositiveLogarithm extends Exception {
    public NonPositiveLogarithm(String msg) {
      super(msg);
    }
  }

  public static Double div(Double x, Double y) throws DivisionByZero {
    if (y == 0)
      throw new DivisionByZero("");
    else
      return x / y;
  }

  public static Double sqrt(Double x) throws NegativeSquareRoot {
    if (x < 0)
      throw new NegativeSquareRoot("");
    else
      return Math.sqrt(x);
  }

  public static Double ln(Double x) throws NonPositiveLogarithm {
    if (x <= 0)
      throw new NonPositiveLogarithm("");
    else
      return Math.log(x);
  }

  public static ZIO3<Double, DivisionByZero, NonPositiveLogarithm, NegativeSquareRoot> getCalculationAsZIO(Double x,
      Double y) {

    return
      sequence(
        () -> div(x, y),
        RustArithmetic::ln,
        RustArithmetic::sqrt
      );
  }
}
