import static augmented.augmentJ.*;
import java.util.List;
import static org.junit.Assert.assertEquals;
import org.junit.Test;

import static examples.AccountHandler.*;
import static examples.Timer.*;
import scala.util.*;

public class ZioExampleJSuite {

  @Test
  public void Timer() {
    var res1 = result(getTimerIO(List.of("3", "10", "100")));
    var res2 = result(getTimerIO(List.of("3", "a")));
    var res3 = result(getTimerIO(List.of("a", "10")));
    var res4 = result(getTimerIO(List.of("4", "10")));
    assertEquals(res1, new Left(FailureReason.InvalidNumberOfArgs));
    assertEquals(res2, new Left(FailureReason.InvalidIntForGain));
    assertEquals(res3, new Left(FailureReason.InvalidIntForMinutes));
  }

  @Test
  public void Account() {
    var prog = getAccountIO();
    result(prog);
  }
}
