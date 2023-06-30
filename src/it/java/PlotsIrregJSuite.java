import static augmented.augment.*;
import static augmented.augmentJ.*;
import basicdef.Color;
import multiarray.ArrayB;
import static multiarray.multiArray.*;
import multiarray.MultiArrayB;
import static multiarrayplot.PlotExtensionsSeq.*;
import static plotutil.imageSrcs.*;
import static util.JavaUtil.*;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.function.*;
import java.util.List;
import java.util.stream.*;
import static org.junit.Assert.*;
import org.junit.Test;

public class PlotsIrregJSuite {

  public static <T> void printSeq(List<List<T>> seq) {
    var r = seq.stream().map(x -> x.stream()
      .map(y -> y.toString())
      .collect(Collectors.joining(" ")))
      .collect(Collectors.joining("\n"));
    println("\n" + r + "\n\n");
  }

  @Test
  public void PascalsTriangle() {
    var binomialCoefficient = seq(
      (Integer x, Integer y) -> org.apache.commons.math3.util.CombinatoricsUtils.binomialCoefficient(x, y));
    var n = 5;
    var res = binomialCoefficient.applySeq(rangeTo(0, n), x -> rangeTo(0, x));
    printSeq(res);
  }

  @Test
  public void tetrahedron() {
    var n = 5;
    var res = select(rangeTo(1, n), a -> rangeTo(1, a), b -> rangeTo(1, b));
    plotTriples(res, "Tetra [J]", 5, 25, false, new Color("green"), false);
  }

  @Test
  public void tetrahedronAnimated() {
    var n = 5;
    var res = select(rangeTo(1, n), a -> rangeTo(1, a), b -> rangeTo(1, b));
    animateTriples(res, "Tetra (animated) [J]", 5, 0, false, new Color("yellow"), false);
  }

  @Test
  public void triangle2Dprinted() {
    var n = 5;
    var r2 = select(rangeTo(1, n), a -> rangeTo(1, a), (a, b) -> true);
    plotPairs(r2, "Triangle [J]", 15, 25, true, new Color("green"), false);
    animatePairs(r2, "Triangle (animated) [J]", 15, 25, true, new Color("yellow"), false);
  }

  @Test
  public void sphere3Dprinted() {
    var r = 5;
    var sphere = select(rangeTo(-r, r), rangeTo(-r, r), rangeTo(-r, r),
      (a, b, c) -> a * a + b * b + c * c <= r * r);
    animateTriples(sphere, "3D-printed sphere [J]", 3, 0, false, new Color("lightblue"), false);
  }

  public static <T> List<T> prepend0(T a, List<T> l) {
    return Stream.concat(List.of(a).stream(), l.stream()).collect(Collectors.toList());
  }

  public static List<List<Integer>> placeQueens(int n, int k) {
    var prepend = augment((Integer a, List<Integer> l) -> prepend0(a, l));

    List<List<Integer>> res = k == 0 ? List.of(new ArrayList<Integer>())
      : prepend.apply(rangeTo(0, n - 1), placeQueens(n, k - 1), (x, qu) -> isSafe(x, qu));
    return res;
  }

  public static Boolean isSafe(int col, List<Integer> queens) {
    var row = queens.size();
    var queensWithRow = zip(rangeTo(row - 1, 0, -1), queens);
    var res = queensWithRow.stream().allMatch(
      pr -> {
        var r = pr.first();
        var c = pr.second();
        return col != c && Math.abs(col - c) != row - r;
      });
    return res;
  }

  @Test
  public void queens() {
    var n = 8;
    var res = placeQueens(n, n);
    var res1 = map(res, l -> map(l, x -> map(range(0, n), a -> a == x ? 1 : 0)));
    var arr = multiarray(range(0, res.size()), rangeTo(1, n), map(rangeTo(1, n), x -> (char) (x + 64)), res1);
    arr.animate("Queens [J]", true, 750, false, some(chessImageSrcs()),
      (x, y) -> (x + y) % 2 == 1 ? "#fccc9c" : "#d48c44");
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
