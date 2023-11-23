import static augmented.augment.*;
import static augmented.augmentJ.*;
import basicdef.*;
import multiarray.*;
import static multiarray.multiArray.*;
import static multiarrayplot.PlotExtensionsSeq.*;
import static plotutil.imageSrcs.*;
import static util.JavaUtil.*;

import info.debatty.java.stringsimilarity.interfaces.StringDistance;
import info.debatty.java.stringsimilarity.*;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.function.*;
import java.util.List;
import java.util.stream.*;
import org.apache.commons.math3.analysis.polynomials.PolynomialFunction;
import org.apache.commons.math3.analysis.function.Gaussian;
import org.apache.commons.math3.fitting.*;
import static org.junit.Assert.*;
import org.junit.Test;

public class PlotsRectJSuite {

  @Test
  public void curveFitting() {
    var obs = new WeightedObservedPoints();
    var xs = List.of(4.0254623, 4.03128248, 4.03839603, 4.04421621, 4.05132976, 4.05326982,
      4.05779662, 4.0636168, 4.06943698, 4.07525716, 4.08237071, 4.08366408);
    var ys = List.of(531026.0, 984167.0, 1887233.0, 2687152.0, 3461228.0, 3580526.0,
      3439750.0, 2877648.0, 2175960.0, 1447024.0, 717104.0, 620014.0);

    IntStream.range(0, xs.size()).forEach(i -> obs.add(xs.get(i), ys.get(i)));
    var obsMap = toMap(xs, ys);
    var paramsGaussian = GaussianCurveFitter.create().fit(obs.toList());
    var paramsPolynomial = PolynomialCurveFitter.create(3).fit(obs.toList());
    var gf = new Gaussian(paramsGaussian[0], paramsGaussian[1], paramsGaussian[2]);
    var pf = new PolynomialFunction(paramsPolynomial);

    var namedFns = new ArrayList<Named<Function<Double, Double>>>();
    namedFns.add(namedFn(x -> obsMap.get(x), "orig"));
    namedFns.add(namedFn(x -> pf.value(x), "fittedPolynomial"));
    namedFns.add(namedFn(x -> gf.value(x), "fittedGaussian"));

    var applyNamedFn = augment((Named<Function<Double, Double>> named, Double x) -> named.mainValue().apply(x));
    var res = applyNamedFn.apply(namedFns, xs);
    res.plotFlat("Fitted Curves [J]", false, "", true);
  }

  @Test
  public void stringDist() {
    var sts = List.of("ABCDEF", "ABDCEF", "BACDFE", "ABCDE", "BCDEF", "ABCGDEF", "WXYZ");
    var sts2 = List.of("ABCDEF", "ABDCEF", "ABACUS", "BACKUP");

    var stringDist = augment((StringDistance sd, String s1, String s2) -> sd.distance(s1, s2));
    var metrics = List.of(new Damerau(), new Jaccard(), new JaroWinkler(), new Levenshtein(), new MetricLCS(),
      new OptimalStringAlignment(), new QGram(), new SorensenDice());

    var res = stringDist.apply(metrics, sts, sts2);
    res.plot("String Distances [J]", false, "", true);
  }

  public static String pulsar() {
    return "000000000000000\n" +
      "000111000111000\n" +
      "000000000000000\n" +
      "010000101000010\n" +
      "010000101000010\n" +
      "010000101000010\n" +
      "000111000111000\n" +
      "000000000000000\n" +
      "000111000111000\n" +
      "010000101000010\n" +
      "010000101000010\n" +
      "010000101000010\n" +
      "000000000000000\n" +
      "000111000111000\n" +
      "000000000000000";
  }

  @Test
  public void cellular() {
    var boardSt = pulsar();
    var board = Stream.of(
      boardSt.trim().split("\n"))
      .map(x -> x.trim().chars().mapToObj(c -> c - 48)
        .collect(Collectors.toList()))
      .collect(Collectors.toList());
    Collections.reverse(board);
    var arr = multiarray(rangeTo(0, 0), range(0, board.size()), range(0, board.get(0).size()), List.of(board));
    var res = nextStep(arr, 50);
    res.animate("Pulsar [J]", true, 750, false, false, (x, y) -> "green");
  }

  public static <T> T nextStepStatus(List<Integer> s, T t, T f) {
    var current = s.get(s.size() / 2);
    var nbNeighbors = s.stream().mapToInt(Integer::intValue).sum() - current;
    var res = nbNeighbors == 3 ? t
      : (current == 1 && nbNeighbors == 2) ? t : f;
    return res;
  }

  public static MultiArrayC<Integer, Integer, Integer, Integer> nextStep(
      MultiArrayC<Integer, Integer, Integer, Integer> arr, Integer n) {
    var a = arr.subArray(arr.lengths(0) - 1);

    if (n == 0)
      return arr;
    else {
      var defaultValue = 0;
      var f = augment((Integer i, Integer j, Integer da,
        Integer db) -> a.get(i + da, j + db, defaultValue) != defaultValue ? 1 : 0);
      var arrD = f.apply(range(0, a.la()), range(0, a.lb()), rangeTo(-1, 1), rangeTo(-1, 1));
      var arrB = arrD.map(s -> nextStepStatus(s, 1, 0));
      int i = arr.lengths(0) + 1;
      var arrC = multiarray(range(0, i), range(0, a.la()), range(0, a.lb()), append(arr.nested(), List.of(arrB.nested())));
      return nextStep(arrC, n - 1);
    }
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
