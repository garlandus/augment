import augmented._
import augmented.given
import augmented.Extensions._
import basicdef._
import mappable._
import mappable.given
import multiarray._
import multiarrayplot.PlotExtensionsSeq._
import util._
import util.StringExtensions.stripMrgn

import info.debatty.java.stringsimilarity.interfaces.StringDistance
import info.debatty.java.stringsimilarity.*
import org.apache.commons.math3.analysis.polynomials.PolynomialFunction
import org.apache.commons.math3.analysis.function.Gaussian
import org.apache.commons.math3.fitting._
import org.apache.commons.math3.util._
import scala.collection.JavaConverters._

class PlotsRectSuite extends munit.FunSuite:

  test("curve fitting"):
    val obs = WeightedObservedPoints()
    val xs = List(4.0254623, 4.03128248, 4.03839603, 4.04421621, 4.05132976, 4.05326982, 4.05779662, 4.0636168,
      4.06943698, 4.07525716, 4.08237071, 4.08366408)
    val ys = List(531026.0, 984167.0, 1887233.0, 2687152.0, 3461228.0, 3580526.0, 3439750.0, 2877648.0, 2175960.0,
      1447024.0, 717104.0, 620014.0)
    (xs zip ys).foreach(obs.add(_, _))

    val prs = obs.toList.asScala.map(pt => (pt.getX(), pt.getY())).toList
    val xVals = xs
    val obsMap = prs.toMap
    val paramsGaussian = GaussianCurveFitter.create().fit(obs.toList)
    val paramsPolynomial = PolynomialCurveFitter.create(3).fit(obs.toList)

    val gf = Gaussian(paramsGaussian(0), paramsGaussian(1), paramsGaussian(2))
    val pf = PolynomialFunction(paramsPolynomial)

    val namedFns = List(
      Named(obsMap, "orig"),
      Named(pf.value(_: Double), "fittedPolynomial"),
      Named(gf.value(_: Double), "fittedGaussian")
    )

    val applyNamedFn = (f: Named[Double => Double], x: Double) => f.mainValue(x)
    val res = applyNamedFn(namedFns, xVals)
    res.plotFlat("Fitted Curves")

  test("string distances"):
    val sts = List("ABCDEF", "ABDCEF", "BACDFE", "ABCDE", "BCDEF", "ABCGDEF", "WXYZ")
    val sts2 = List("ABCDEF", "ABDCEF", "ABACUS", "BACKUP")

    val metrics = List(
      Damerau(),
      Jaccard(),
      JaroWinkler(),
      Levenshtein(),
      MetricLCS(),
      OptimalStringAlignment(),
      QGram(),
      SorensenDice()
    )
    val namedFns = metrics.map(x => Named(x, x.getClass.getSimpleName))

    val stringDist = (sd: Named[StringDistance], s1: String, s2: String) => sd.mainValue.distance(s1, s2)
    val res = stringDist(namedFns, sts, sts2)
    res.plot("String Distances")

  val pulsar = """
    000000000000000
    000111000111000
    000000000000000
    010000101000010
    010000101000010
    010000101000010
    000111000111000
    000000000000000
    000111000111000
    010000101000010
    010000101000010
    010000101000010
    000000000000000
    000111000111000
    000000000000000
    """.stripMrgn

  test("cellular"):
    val boardSt = pulsar
    val board = stringAsIntArr(boardSt)
    val arr = multiArray(0 to 0, 0 until board.length, 0 until board.head.length, List(board.map(_.toList).toList))
    val res = nextStep(arr)(50)
    res.animate("Pulsar", true, 750, addTimeStamp = false, visibleAxes = false)

  def f0[A](a: MultiArrayB[A, Int, Int])(defaultValue: A)(i: Int, j: Int, da: Int, db: Int) =
    if a.get(i + da, j + db, defaultValue) != defaultValue then 1 else 0

  def f1[A](a: MultiArrayB[A, Int, Int])(defaultValue: A) = f0(a)(defaultValue)

  def nextStepStatus[A](t: A, f: A)(s: Seq[Int]) =
    val current = s(s.length / 2)
    val nbNeighbors = s.sum - current
    (current, nbNeighbors) match
      case (_, 3) => t
      case (1, 2) => t
      case _      => f

  def nextStep[A](arr: MultiArrayC[Int, Int, Int, Int])(n: Int): MultiArrayC[Int, Int, Int, Int] =
    val a = arr.subArray(arr.as.length - 1)
    n match
      case 0 => arr
      case _ =>
        val arrD = f1(a)(0)(0 until a.la, 0 until a.lb, -1 to 1, -1 to 1)
        val arrB = arrD.map(nextStepStatus(1, 0))
        val i = arr.as.length + 1
        val arrC = multiArray(0 until i, 0 until a.la, 0 until a.lb, arr.nested() ++ List(arrB.nested()))
        nextStep(arrC)(n - 1)
