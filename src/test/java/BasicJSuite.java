import static augmented.augmentJ.*;
import static mappable.Mapper.*;
import static multiarray.multiArray.*;
import static util.JavaUtil.complement;
import static util.JavaUtil.filter;
import static util.JavaUtil.Pair;
import static util.JavaUtil.stringAsIntSeq;
import static util.JavaUtil.subArray;
import static util.JavaUtil.toMap;
import static util.JavaUtil.Triple;
import static util.JavaUtil.zip;
import multiarray.ArrayB;
import multiarray.MultiArrayB;

import static java.util.stream.IntStream.range;
import static java.util.stream.IntStream.rangeClosed;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.function.*;
import java.util.List;
import java.util.Optional;
import java.util.stream.*;

import scala.util.Try;
import org.junit.Test;
import static org.junit.Assert.*;

public class BasicJSuite {

  static Triple tr(Integer a, Integer b, Integer c) {
    return new Triple(a, b, c);
  }

  @Test
  public void PythagoreanTriangles() {
    var n = 20;

    var res = select(range(1, n), a -> range(a, n), b -> range(b, n), (a, b, c) -> a * a + b * b == c * c);

    assertEquals(res,
      List.of(
        tr(3, 4, 5),
        tr(5, 12, 13),
        tr(6, 8, 10),
        tr(8, 15, 17),
        tr(9, 12, 15)));
  }

  @Test
  public void tetrahedron() {
    var n = 5;

    var res = count(rangeClosed(1, n), a -> rangeClosed(1, a), b -> rangeClosed(1, b));

    assertEquals(res, 35);
  }

  @Test
  public void basicB() {
    var mult = augment((Integer a, Integer b) -> a * b);
    var add = augment((Integer a, Integer b, Integer c) -> a + b + c);
    var as = rangeClosed(3, 5);
    var bs = rangeClosed(7, 9);
    var res = mult.apply(as, bs);
    assertEquals(res.nestedAsJava(), List.of(List.of(21, 24, 27), List.of(28, 32, 36), List.of(35, 40, 45)));

    var sts1 = List.of("ab", "cd", "ef");
    var sts2 = List.of("gh", "ij", "kl");
    var appendAsterisks = augment((String s, Integer n) -> s + "*".repeat(n));
    var res1 = appendAsterisks.apply(sts1, rangeClosed(3, 5));

    assertEquals(res1.nestedAsJava(), List.of(List.of("ab***", "ab****", "ab*****"),
      List.of("cd***", "cd****", "cd*****"), List.of("ef***", "ef****", "ef*****")));

    var as1 = rangeClosed(3, 5);
    var bs1 = rangeClosed(7, 9);
    var g = augment(
      (List<String> sts, List<Integer> ls) -> sts.stream().collect(Collectors.joining(" ")) + " "
        + ls.stream().mapToInt(Integer::intValue).sum());
    var res2 = g.apply(List.of(sts1, sts2), List.of(as1.boxed().toList(), bs1.boxed().toList()));
    assertEquals(res2.nestedAsJava(),
      List.of(List.of("ab cd ef 12", "ab cd ef 24"), List.of("gh ij kl 12", "gh ij kl 24")));
  }

  @Test
  public void basicC() {
    var as = rangeClosed(3, 4);
    var bs = rangeClosed(7, 8);
    var f = augment((Integer x, Integer y, String s) -> s + ": " + x * y);
    var res = f.apply(as, bs, List.of("acorn", "beetroot"));
    assertEquals(res.nestedAsJava(),
      List.of(List.of(List.of("acorn: 21", "beetroot: 21"), List.of("acorn: 24", "beetroot: 24")),
        List.of(List.of("acorn: 28", "beetroot: 28"), List.of("acorn: 32", "beetroot: 32"))));
  }

  public static class Person {
    public String name;
    public boolean isMale;
    public List<Person> children;

    public Person(String name, boolean isMale, List<Person> children) {
      this.name = name;
      this.isMale = isMale;
      this.children = children;
    }

    public Person(String name, boolean isMale) {
      this(name, isMale, new ArrayList<Person>());
    }
  }

  @Test
  public void persons() {
    var lara = new Person("Lara", false);
    var joe = new Person("Joe", true);
    var bob = new Person("Bob", true, List.of(joe));
    var julie = new Person("Julie", false, List.of(lara, bob));
    var persons = List.of(lara, bob, julie);
    var getNames = augment((Person p, Person c) -> List.of(p.name, c.name));

    var names = getNames.apply(filter(persons, p -> !p.isMale), p -> p.children);

    assertEquals(names, List.of(List.of("Julie", "Lara"), List.of("Julie", "Bob")));
  }

  @Test
  public void permutations() {
    var l = new String[] { "A", "B", "C", "D" };
    var res0 = permA(Arrays.asList(l));
    var res = res0.stream().map(x -> x.stream()
      .collect(Collectors.joining("")))
      .collect(Collectors.joining(" "));
    assertEquals(res,
      "ABCD ABDC ACBD ACDB ADBC ADCB BACD BADC BCAD BCDA BDAC BDCA CABD CADB CBAD CBDA CDAB CDBA DABC DACB DBAC DBCA DCAB DCBA");
  }

  public static <T> List<T> prepend0(T a, List<T> l) {
    return Stream.concat(List.of(a).stream(), l.stream()).collect(Collectors.toList());
  }

  public static <T> List<T> diff(List<T> l, T t) {
    return l.stream()
      .filter(x -> x != t)
      .collect(Collectors.toList());
  }

  public static <T> List<List<T>> permA(List<T> x) {
    var prepend = augment((T a, List<T> l) -> prepend0(a, l));

    List<List<T>> res = x.isEmpty() ? List.of(new ArrayList<T>())
      : prepend.apply(x, a -> permA(diff(x, a)));
    return res;
  }

  public static Integer sqrt(int x) {
    return (int) (Math.sqrt(x));
  }

  public static IntStream sieveA(int n) {
    Function<Integer, Integer> sqrt = ((Integer x) -> (int) (Math.sqrt(x)));
    var mult = augment((Integer x, Integer y) -> x * y);

    var compositeNbs = mult.apply(rangeClosed(2, sqrt(n)), a -> rangeClosed(2, n / a));
    return complement(compositeNbs, rangeClosed(2, n));
  }

  public static IntStream sieveB(int n) {
    var mult = augment((Integer x, Integer y) -> x * y);
    return n == 1 ? IntStream.empty()
      : complement(mult.apply(sieveB(sqrt(n)), a -> rangeClosed(2, n / a)), rangeClosed(2, n));
  }

  public static <T> List<T> until(boolean b, List<T> l) {
    return b ? new ArrayList<T>()
      : l;
  }

  public static IntStream sieveC(int n) {
    var mult = augment((Integer x, Integer y) -> x * y);
    return n == 1 ? IntStream.empty()
      : complement(mult.apply(sieveC(sqrt(n)), a -> rangeClosed(a, n / a)), rangeClosed(2, n));
  }

  @Test
  public void sieves() {
    var n = 50;
    var resA = sieveA(n).boxed().toList();
    var resB = sieveB(n).boxed().toList();
    var resC = sieveC(n).boxed().toList();
    println("\n" + resA + "\n");
    println(resB + "\n");
    println(resC + "\n");
    assertEquals(resB, resA);
    assertEquals(resC, resA);
  }

  public static List<List<Integer>> placeQueens(int n, int k) {
    var prepend = augment((Integer a, List<Integer> l) -> prepend0(a, l));

    List<List<Integer>> res = k == 0 ? List.of(new ArrayList<Integer>())
      : prepend.apply(range(0, n), placeQueens(n, k - 1), (x, qu) -> isSafe(x, qu));
    return res;
  }

  public static Boolean isSafe(int col, List<Integer> queens) {
    var row = queens.size();
    var queensWithRow = zip(range(0, row).map(x -> row - x - 1), queens);
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

    assertEquals(res.size(), 92);
    assertEquals(res.get(0), List.of(0, 4, 7, 5, 2, 6, 1, 3));
  }

  public static String board() {
    return "003020600\n" +
      "900305001\n" +
      "001806400\n" +
      "008102900\n" +
      "700000008\n" +
      "006708200\n" +
      "002609500\n" +
      "800203009\n" +
      "005010300\n";
  }

  @Test
  public void sudoku() {
    var bn = stringAsIntSeq(board());
    var a = multiarray(rangeClosed(1, 9), rangeClosed(1, 9), bn);
    println("\nInitial board:" + a.toString(x -> x == 0 ? "." : x.toString()) + "\n");
    var x = a.selectPairs(z -> z == 0);
    var r = sudo(a, x);
    var bds = completedBoards(a, x, r);
    println("Completed board:\n" + bds + "\n\n");
    assertEquals(bds.size(), 1);
    assertEquals(bds.get(0).nestedAsJava(),
      List.of(
        List.of(4, 8, 3, 9, 2, 1, 6, 5, 7), List.of(9, 6, 7, 3, 4, 5, 8, 2, 1),
        List.of(2, 5, 1, 8, 7, 6, 4, 9, 3), List.of(5, 4, 8, 1, 3, 2, 9, 7, 6),
        List.of(7, 2, 9, 5, 6, 4, 1, 3, 8), List.of(1, 3, 6, 7, 9, 8, 2, 4, 5),
        List.of(3, 7, 2, 6, 8, 9, 5, 1, 4), List.of(8, 1, 4, 2, 5, 3, 7, 6, 9),
        List.of(6, 9, 5, 4, 1, 7, 3, 8, 2)));
  }

  public static List<List<Integer>> sudo(ArrayB<Integer> a, List<Pair<Integer, Integer>> bl) {
    var prepend = augment((Integer x, List<Integer> l) -> prepend0(x, l));

    List<List<Integer>> r = bl.isEmpty() ? List.of(new ArrayList<Integer>())
      : prepend.apply(rangeClosed(1, 9), sudo(a, bl.stream().skip(1).toList()), (n, y) -> isValidSudoku(a, bl, n, y));
    return r;
  }

  public static boolean isValidSudoku(ArrayB<Integer> a, List<Pair<Integer, Integer>> bl, int n, List<Integer> y) {
    var remainingPairs = bl.stream().skip(1).collect(Collectors.toList());
    var arr = placedValsToArray(zip(remainingPairs, y), 9, 9);

    // completed board
    var bd = arr.plus(a);
    var ij = bl.get(0);
    var si = (ij.first() - 1) / 3;
    var sj = (ij.second() - 1) / 3;
    var sa = subArray(bd, si, sj, 3, 3);
    var isInSubSquare = sa.stream().anyMatch(x -> x.contains(n));

    var r = bd.row(ij.first());
    var c = bd.col(ij.second());
    var isInRowOrCol = r.contains(n) || c.contains(n);
    return !(isInSubSquare || isInRowOrCol);
  }

  public static List<MultiArrayB<Integer, Integer, Integer>> completedBoards(ArrayB<Integer> a,
      List<Pair<Integer, Integer>> bl, List<List<Integer>> values) {
    return
      values
        .stream()
        .map(v -> zip(bl, v))
        .map(x -> placedValsToArray(x, 9, 9))
        .map(x -> x.plus(a))
        .toList();
  }

  public static MultiArrayB<Integer, Integer, Integer> placedValsToArray(
      List<Pair<Pair<Integer, Integer>, Integer>> v, int m, int n) {
    var mp = toMap(v);

    var ll = rangeClosed(1, n).mapToObj(
      r -> rangeClosed(1, m).mapToObj(
        c -> mp.getOrDefault(new Pair<Integer, Integer>(r, c), 0)));
    return multiarray(rangeClosed(1, n), rangeClosed(1, n), ll);
  }

  public static <T> void printSeq(List<List<T>> seq) {
    println(seq);
    var res = seq.stream().map(x -> x.stream()
      .map(y -> y.toString())
      .collect(Collectors.joining(" ")))
      .collect(Collectors.joining("\n"));
    println("\n" + res + "\n\n");
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
