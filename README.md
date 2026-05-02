# augment

## Augmented functions

Augmenting a function means implementing "type constructor polymorphism": if its original argument types were A, B and C, it will now be applicable to arguments of type T[A, B, C], for a wide range of values of T.

## Overview

The additional behavior depends on the value of T: this can go from simple vectorization (T = Vector) as found in R and MATLAB, to representating monadic chains, in other words doing the work of a for-comprehension (or nested flatMap calls), to representing "near-monadic chains" that in fact imply the use of a monad transformer.

## Quick start

(For Java and Clojure, setup and examples follow further below.)\
In Scala, you can add the following to build.sbt:
```scala
libraryDependencies += "co.computist" %% "augment" % "0.0.4"
```

Imports to get you started:
```scala
import augmented._
import augmented.given
import mappable.given
import mappablethirdparty.given   // for Cats Effect, ZIO, Guava, etc
```

If you want all (true) functions to be augmented by default:
```scala
import augmented.Extensions._
```

(Note that Scala methods will not be affected by default. There are a number of simple ways to convert a method to a function, including assigning it to a variable, or following it with an underscore.)

## Why augmented function notation is often the most concise

Augmented function calls can fully replace for-comprehensions, and are often more concise. To see how, let's look at the example of Pythagorean triples, starting with a for-comprehension:

```scala
case class Triangle(a: Int, b: Int, c: Int)
val toTriangle = Triangle.apply

def getThirdLength(a: Int, b: Int) = (b to n).filter(c => a * a + b * b == c * c)

val trianglesA =
  for
    a <- 1 to n
    b <- a to n
    c <- getThirdLength(a, b)
  yield toTriangle(a, b, c)
```

(Here we've avoided the use of a separate guard, which might slightly muddy the waters.)

The flatMap equivalent is then

```scala
val trianglesB =
  (1 to n).flatMap(
    a => (a to n).flatMap(
      b => getThirdLength(a, b).map(
        c => toTriangle(a, b, c))))
```
Now written as an augmented function call:


```scala
val trianglesC = toTriangle(1 to n, _ to n, getThirdLength)
```

Considerably shorter... but we can "reinflate" it to get back to something resembling both the for-comprehension and the nested flatMaps.
We can start by using `sequence`, which will yield the value of the last element in the chain:

```scala
val trianglesD = sequence(1 to n, _ to n, getThirdLength, toTriangle)
```

We can then simply write this a bit differently, without changing its meaning at all:

```scala
val trianglesE = sequence(
                1 to n,
  a         =>  a to n,
  (a, b)    =>  getThirdLength(a, b),
  (a, b, c) =>  toTriangle(a, b, c)
)
```
In doing so we see that the terms in both the for-comprehension and the nested flatMaps reappear: `1 to n`, `a to n`, `getThirdLength(a, b)` and `toTriangle(a, b, c)`. The original form had eliminated all the extras, and kept only the essential information that characterizes the chain.


## Other Examples

### Pascal's triangle

```scala
binomialCoefficient(0 to n, 0 to _)
```

### Tetrahedron

```scala
select(1 to n, 1 to _, 1 to _)
```

### ZIO

There are some ZIO examples (in the `examples` folder), adapted from Alvin Alexander's tutorials (https://www.learnscala.dev/challenge-page/zio-2-functional-programming-fundamentals-course), that can show you the difference in syntax for more "real-world" cases. They also illustrate how the `sequence` syntax stays almost the same between Scala and Java: it's just function application.

## From Java

### Quick start

You can add the following to the dependencies in a *pom.xml* file:
```xml
<dependency>
  <groupId>co.computist</groupId>
  <artifactId>augment_3</artifactId>
  <version>0.0.4</version>
</dependency>
<dependency>
  <groupId>org.scala-lang</groupId>
  <artifactId>scala3-library_3</artifactId>
  <version>3.5.2</version>
</dependency>  
```

Imports to get you started:
```java
import static augmented.augmentJ.*;
import static java.util.stream.IntStream.range;
```

Although Java 8 is sufficient, Java 11 is recommended since *var* means you can avoid lengthy explicit type names (often with multiple generic parameters).

### Examples

### Pythagorean triples

This can be compared with e.g. https://rosettacode.org/wiki/List_comprehensions#Java

```java
var n = 20;

var triangles =
  select(
    range(1, n),
    a -> range(a, n),
    b -> range(b, n),
    (a, b, c) -> a * a + b * b == c * c);     // [[3 4 5], [5 12 13], [6 8 10], [8 15 17], [9 12 15]]
```

### Propagation of future values

```java
import static mappable.Mapper.mappable;

var mult = augment((Integer a, Integer b) -> a * b);
var add = augment((Integer a, Integer b, Integer c) -> a + b + c);

mult.apply(4, 5);   // here mult returns an ordinary value (20)
add.apply(4, 5, 6); // 15

var executor = Executors.newSingleThreadExecutor();
var futureVal = mappable(4, a -> executor.submit(() -> {Thread.sleep(500); return a;}));

var x = mult.apply(futureVal, 5);
var y = add.apply(2, x, 3);
var z = mult.apply(4, y);   // here mult returns a future value

assertEquals(z.mappable() instanceof FutureTask, true);
assertEquals(z.hasValue(), false);
Thread.sleep(1000);
assertEquals(z.hasValue(), true);
assertEquals(z.value(), (Integer) 100);
```

### ZIO

Please see the comments about ZIO examples in the Scala section, above.


## From Clojure

### Quick start

You can add the following to the dependencies in a *project.clj* file:
```clojure
[co.computist/augment_3 "0.0.4"]
[org.scala-lang/scala3-library_3 "3.5.2"]
```

### Examples

### Pythagorean triples

```clojure
(defn augment [f] (augmentedClj.augment/apply f))
(def triple (augment (fn [a b c] [a b c])))
(def n 20)

(def triples
  (triple
    (range 1 n)
    #(range % n)
    #(range % n)
    (fn [a b c] (= (+ (* a a) (* b b)) (* c c)))))

(is (= triples [[3 4 5] [5 12 13] [6 8 10] [8 15 17] [9 12 15]]))
```

### Function graph

```clojure
(def squares (augment (fn [a b] (- 100 (+ (* a a) (* b b))))))
(squares 5 5)                                      ; 50
(.graph (squares (range -10 11) (range -10 11)))   ; plots function using HTML / JavaScript / plotly
```
<img src="img/graph.png" width="500" height="250"/>

### Propagation of future values

```clojure
(defn mappable [x] (augmentedClj.Mapper/mappable x))

;; this returns a Clojure function, i.e. one that implements IFn
(def mult (augment (fn [a b] (* a b))))
(def add (augment (fn [a b c] (+ a b c))))

(mult 4 5)  ; here mult returns an ordinary value (20)
(add 4 5 6) ; 15

(def futureVal (mappable (future (Thread/sleep 500) (println "done") (+ 1 3))))

(def x (mult futureVal 5))
(def y (add 2 x 3))
(def z (mult 4 y))  ; here mult returns a future value

(is (= (type (. z mappable)) FutureTask))
(is (= (. z hasValue) false))
(Thread/sleep 1000)
(is (= (. z hasValue) true))
(is (= (. z value) 100))
```
