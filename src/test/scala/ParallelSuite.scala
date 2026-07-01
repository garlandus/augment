import augmented._
import augmented.Extensions._
import comprehension.parallel.given
import mappable.given
import multiarray._
import shape._
import util.timed

class ParallelSuite extends munit.FunSuite:

  test("Comprehension: parallel rectangular speedup (arity 2)"):
    val delay = 50
    def delayedProd = (a: Int, b: Int) =>
      Thread.sleep(delay)
      a * b

    val n = 4
    val (res, t) = timed(delayedProd(1 to n, 1 to n))
    val isParallel = t < n * n * delay * 1000 / 2 // µs

    assertEquals(res.flat().length, n * n)
    assertEquals(isParallel, true)

  test("Comprehension: parallel rectangular speedup (arity 4)"):
    val delay = 25
    def delayedSum = (a: Int, b: Int, c: Int, d: Int) =>
      Thread.sleep(delay)
      a + b + c + d

    val xs = 1 to 2
    val cells = math.pow(2, 4).toInt

    val (res, t) = timed(delayedSum(xs, xs, xs, xs))
    val isParallel = t < cells * delay * 1000 / 2 // µs

    assertEquals(res.flat().length, cells)
    assertEquals(isParallel, true)
