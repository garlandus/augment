package util;

import java.util.List;
import java.util.function.*;
import java.util.stream.Collectors;

import scala.util.*;

public class AugmentUtil {

  public static class Unit {
  };

  @FunctionalInterface
  public interface Fn0<R> {
    R apply();
  }

  @FunctionalInterface
  public interface Fn1<T, R> {
    R apply(T t);
  }

  @FunctionalInterface
  public interface Fn2<T, U, R> {
    R apply(T t, U u);
  }

  @FunctionalInterface
  public interface Fn3<T, U, V, R> {
    R apply(T t, U u, V v);
  }

  @FunctionalInterface
  public interface ThrowingFnV0<E1 extends Throwable> {
    void apply() throws E1;

    static <E1 extends Throwable> Supplier<Either<E1, Unit>> asEitherFn(ThrowingFnV0<E1> f) {

      return () -> {
        try {
          f.apply();
          return new Right<E1, Unit>(new Unit());
        } catch (Throwable e) {
          return new Left<E1, Unit>((E1) e);
        }
      };
    }
  }

  @FunctionalInterface
  public interface ThrowingFn0<R, E1 extends Throwable> {
    R apply() throws E1;

    static <R, E1 extends Throwable> Supplier<Either<E1, R>> asEitherFn(ThrowingFn0<R, E1> f) {

      return () -> {
        try {
          return new Right<E1, R>(f.apply());
        } catch (Throwable e) {
          return new Left<E1, R>((E1) e);
        }
      };
    }
  }

  @FunctionalInterface
  public interface ThrowingFnV1<T, E1 extends Throwable> {
    void apply(T t) throws E1;

    static <T, E1 extends Throwable> Fn1<T, Either<E1, Unit>> asEitherFn(ThrowingFnV1<T, E1> f) {

      return t -> {
        try {
          f.apply(t);
          return new Right<E1, Unit>(new Unit());
        } catch (Throwable e) {
          return new Left<E1, Unit>((E1) e);
        }
      };
    }
  }

  @FunctionalInterface
  public interface ThrowingFn1<T, R, E1 extends Throwable> {
    R apply(T t) throws E1;

    static <T, R, E1 extends Throwable> Function<T, Either<E1, R>> asEitherFn(ThrowingFn1<T, R, E1> f) {

      return t -> {
        try {
          return new Right<E1, R>(f.apply(t));
        } catch (Throwable e) {
          return new Left<E1, R>((E1) e);
        }
      };
    }
  }

  @FunctionalInterface
  public interface ThrowingFnV2<T, U, E1 extends Throwable> {
    void apply(T t, U u) throws E1;

    static <T, U, E1 extends Throwable> Fn2<T, U, Either<E1, Unit>> asEitherFn(ThrowingFnV2<T, U, E1> f) {

      return (t, u) -> {
        try {
          f.apply(t, u);
          return new Right<E1, Unit>(new Unit());
        } catch (Throwable e) {
          return new Left<E1, Unit>((E1) e);
        }
      };
    }
  }

  @FunctionalInterface
  public interface ThrowingFn2<T, U, R, E1 extends Throwable> {
    R apply(T t, U u) throws E1;

    static <T, U, R, E1 extends Throwable> Fn2<T, U, Either<E1, R>> asEitherFn(ThrowingFn2<T, U, R, E1> f) {

      return (t, u) -> {
        try {
          return new Right<E1, R>(f.apply(t, u));
        } catch (Throwable e) {
          return new Left<E1, R>((E1) e);
        }
      };
    }
  }

  public static void println() {
    java.lang.System.out.println();
  }

  public static void println(String s) {
    java.lang.System.out.println(s);
  }

  public static <A> void println(A a) {
    java.lang.System.out.println(a.toString());
  }

  public static Unit printlnUnit(String s) {
    java.lang.System.out.println(s);
    return new Unit();
  }

  public static <T> void printSeq(List<List<T>> seq) {
    println(seq);
    var res = seq.stream().map(x -> x.stream().map(y -> y.toString()).collect(Collectors.joining(" ")))
        .collect(Collectors.joining("\n"));
    println("\n" + res + "\n\n");
  }
}
