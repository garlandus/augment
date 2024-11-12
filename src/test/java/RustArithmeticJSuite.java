import static augmented.augmentJ.*;

import org.junit.Test;
import static org.junit.Assert.*;
import static examples.RustArithmetic.*;

public class RustArithmeticJSuite {

  @Test
  public void Arithmetic() {
    var calc1 = getCalculationAsZIO(100.0, 10.0);
    var calc2 = getCalculationAsZIO(1.0, -1.0);
    var calc3 = getCalculationAsZIO(1.0, 10.0);
    var calc4 = getCalculationAsZIO(1.0, 0.0);

    var res1 = value(calc1.io());
    var res2 = result(calc2.io());
    var res3 = result(calc3.io());
    var res4 = result(calc4.io());
    assertEquals((double) res1, Math.sqrt(Math.log(10.0)), 0.000001);
    assertEquals(res2.swap().getOrElse(null).getClass(), NonPositiveLogarithm.class);
    assertEquals(res3.swap().getOrElse(null).getClass(), NegativeSquareRoot.class);
    assertEquals(res4.swap().getOrElse(null).getClass(), DivisionByZero.class);
  }
}
