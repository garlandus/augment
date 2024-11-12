package util;

import static augmented.augmentJ.*;
import util.AugmentUtil.*;

import static scala.jdk.javaapi.FunctionConverters.*;
import scala.util.Either;
import zio.*;

public class ZioUtil {

  public static <E1 extends Throwable> Fn0<ZIO<Object, E1, Unit>> attemptFn(ThrowingFnV0<E1> f) {
    return () -> {
      try {
        f.apply();
        return augmented.augmentJ.<E1, Unit>zioSucceed(new Unit());
      } catch (Throwable e) {
        return augmented.augmentJ.<E1, Unit>zioFail((E1) e);
      }
    };
  };

  public static <R, E1 extends Throwable> ZIO<Object, E1, R> attempt(ThrowingFn0<R, E1> f) {
    return zioFromEither(ThrowingFn0.asEitherFn(f));
  }

  public static <R, E1 extends Throwable> Fn0<ZIO<Object, E1, R>> attemptFn(ThrowingFn0<R, E1> f) {
    return () -> {
      try {
        return augmented.augmentJ.<E1, R>zioSucceed(f.apply());
      } catch (Throwable e) {
        return augmented.augmentJ.<E1, R>zioFail((E1) e);
      }
    };
  };

  public static <R, E1 extends Throwable, E2> Fn0<ZIO<Object, E2, R>> attemptAndChangeException(ThrowingFn0<R, E1> f,
      E2 e) {
    return () -> {
      try {
        return augmented.augmentJ.<E2, R>zioSucceed(f.apply());
      } catch (Throwable exc) {
        return augmented.augmentJ.<E2, R>zioFail(e);
      }
    };
  };

  static <R, E1 extends Throwable> scala.Function0<Either<E1, R>> attemptFnAsScala(ThrowingFn0<R, E1> f) {
    return asScalaFromSupplier(ThrowingFn0.asEitherFn(f));
  }

  public static <T, R, E1 extends Throwable> Fn1<T, ZIO<Object, E1, R>> attemptFn(ThrowingFn1<T, R, E1> f) {
    return t -> {
      try {
        return augmented.augmentJ.<E1, R>zioSucceed(f.apply(t));
      } catch (Throwable e) {
        return augmented.augmentJ.<E1, R>zioFail((E1) e);
      }
    };
  };

  static <T, R, E1 extends Throwable> scala.Function1<T, Either<E1, R>> attemptFnAsScala(ThrowingFn1<T, R, E1> f) {
    return asScalaFromFunction(ThrowingFn1.asEitherFn(f));
  }

  static <T, U, R, E1 extends Throwable> Fn2<T, U, ZIO<Object, E1, R>> attemptFn(ThrowingFn2<T, U, R, E1> f) {
    return (t, u) -> {
      try {
        return augmented.augmentJ.<E1, R>zioSucceed(f.apply(t, u));
      } catch (Throwable e) {
        return augmented.augmentJ.<E1, R>zioFail((E1) e);
      }
    };
  };

  static <T, U, R, E1 extends Throwable> scala.Function2<T, U, Either<E1, R>> attemptFnAsScala(
      ThrowingFn2<T, U, R, E1> f) {
    return asScalaFromBiFunction((t, u) -> ThrowingFn2.asEitherFn(f).apply(t, u));
  }

  public static <A, B, C, E1 extends Exception, E2 extends Exception, E3 extends Exception> ZIO3<C, E1, E2, E3> sequence(
      ThrowingFn0<A, E1> as, ThrowingFn1<A, B, E2> bs, ThrowingFn1<B, C, E3> cs) {
    return sequence(as, bs, (_, b) -> cs.apply(b));
  }

  public static <A, B, C, E1 extends Exception, E2 extends Exception, E3 extends Exception> ZIO3<C, E1, E2, E3> sequence(
      ThrowingFn0<A, E1> as, ThrowingFn1<A, B, E2> bs, ThrowingFn2<A, B, C, E3> cs) {
    var as1 = attemptFnAsScala(as);
    var bs1 = attemptFnAsScala(bs);
    var cs1 = attemptFnAsScala(cs);
    return sequenceEith(as1, bs1, cs1);
  }

  public static <A, B, C, D, E1 extends Exception, E2 extends Exception, E3 extends Exception, E4 extends Exception>
      ZIO4<D, E1, E2, E3, E4> sequence(ThrowingFn0<A, E1> as, ThrowingFn1<A, B, E2> bs, ThrowingFn2<A, B, C, E3> cs, ThrowingFn2<B, C, D, E4> ds) {
    var as1 = attemptFnAsScala(as);
    var bs1 = attemptFnAsScala(bs);
    var cs1 = attemptFnAsScala(cs);
    var ds1 = attemptFnAsScala(ds);
    return sequenceEith(as1, bs1, cs1, ds1);
  }

  public static <A, B, C, D, E, E1 extends Exception, E2 extends Exception, E3 extends Exception, E4 extends Exception, E5 extends Exception>
      ZIO5<E, E1, E2, E3, E4, E5> sequence(ThrowingFn0<A, E1> as, ThrowingFn1<A, B, E2> bs, ThrowingFn2<A, B, C, E3> cs, ThrowingFn2<B, C, D, E4> ds,
        ThrowingFn2<C, D, E, E5> es) {
    var as1 = attemptFnAsScala(as);
    var bs1 = attemptFnAsScala(bs);
    var cs1 = attemptFnAsScala(cs);
    var ds1 = attemptFnAsScala(ds);
    var es1 = attemptFnAsScala(es);
    return sequenceEith(as1, bs1, cs1, ds1, es1);
  }

  public static <E1 extends Exception> ThrowingFn0<Unit, E1> toThrowingFn(ThrowingFnV0<E1> f) {
    return () -> {
      f.apply();
      return new Unit();
    };
  }

  public static <T, E1 extends Exception> ThrowingFn1<T, Unit, E1> toThrowingFn(ThrowingFnV1<T, E1> f) {
    return t -> {
      f.apply(t);
      return new Unit();
    };
  }

  public static <T, U, E1 extends Exception> ThrowingFn2<T, U, Unit, E1> toThrowingFn(ThrowingFnV2<T, U, E1> f) {
    return (t, u) -> {
      f.apply(t, u);
      return new Unit();
    };
  }

  public static <A, B, C, D, E, E1 extends Exception, E2 extends Exception, E3 extends Exception, E4 extends Exception, E5 extends Exception>
      ZIO5<E, E1, E2, E3, E4, E5> sequence(ThrowingFnV0<E1> as, ThrowingFn1<Unit, B, E2> bs, ThrowingFn2<Unit, B, C, E3> cs, ThrowingFn2<B, C, D, E4> ds,
        ThrowingFn2<C, D, E, E5> es) {
    var as1 = toThrowingFn(as);
    return sequence(as1, bs, cs, ds, es);
  }

  public static <A, B, C, D, E, F, E1 extends Exception, E2 extends Exception, E3 extends Exception, E4 extends Exception, E5 extends Exception, E6 extends Exception>
      ZIO6<F, E1, E2, E3, E4, E5, E6> sequence(ThrowingFn0<A, E1> as, ThrowingFn1<A, B, E2> bs, ThrowingFn2<A, B, C, E3> cs, ThrowingFn2<B, C, D, E4> ds,
        ThrowingFn2<C, D, E, E5> es, ThrowingFn2<D, E, F, E6> fs) {
    var as1 = attemptFnAsScala(as);
    var bs1 = attemptFnAsScala(bs);
    var cs1 = attemptFnAsScala(cs);
    var ds1 = attemptFnAsScala(ds);
    var es1 = attemptFnAsScala(es);
    var fs1 = attemptFnAsScala(fs);
    return sequenceEith(as1, bs1, cs1, ds1, es1, fs1);
  }

  public static <B, C, D, F, E1 extends Exception, E2 extends Exception, E3 extends Exception, E4 extends Exception, E5 extends Exception, E6 extends Exception>
      ZIO6<Unit, E1, E2, E3, E4, E5, E6> sequence(ThrowingFnV0<E1> as, ThrowingFn1<Unit, B, E2> bs, ThrowingFn2<Unit, B, C, E3> cs, ThrowingFn2<B, C, D, E4> ds,
        ThrowingFnV2<C, D, E5> es, ThrowingFnV2<D, Unit, E6> fs) {
    var as1 = toThrowingFn(as);
    var es1 = toThrowingFn(es);
    var fs1 = toThrowingFn(fs);
    return sequence(as1, bs, cs, ds, es1, fs1);
  }

  public static <R, E1 extends Exception, E2> ZIO<Object, E2, R> attempt(ThrowingFn0<R, E1> f, E2 e) {
    return attemptAndChangeException(f, e).apply();
  }
}
