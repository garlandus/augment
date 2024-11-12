import augmented._
import augmented.given
import basicdef._
import mappable._
import mappable.given
import multiarray._
import multiarrayplot.PlotExtensionsSeq._
import plotutil._
import shape.SeqExtensions._

class PlotsIrregSuite extends munit.FunSuite:

  def printSeq[A](seq: Seq[Seq[A]]) =
    println(s"${seq.map(_.mkString(" ")).mkString("\n")}\n")

  test("tetrahedron"):
    val n = 5
    val res0 = select(1 to n, 1 to _, 1 to _)
    val res = res0.map((a, b, c) => (c, b, a))
    res.plot("Tetra", sphereSize = 5, visibleAxes = false, color = Color("lightgreen"))

  test("tetrahedron (animated)"):
    val n = 5
    val res = select(1 to n, 1 to _, 1 to _)
    val duration = Durations(List((20, 1), (500, 1), (200, 2), (1000, 1), (100, 5), (1000, 1), (50, 9), (1000, 1)))
    res.animate("Tetra (animated)", sphereSize = 5, visibleAxes = false, duration = duration)

  test("2D-printed triangle"):
    val n = 5
    val res = select(1 to n, 1 to _)
    res.plotB("Triangle", sphereSize = 15, color = Color("lightblue"))
    res.animateB("2D-printed triangle", sphereSize = 15, color = Color("lightblue"))

  test("3D-printed sphere"):
    val r = 5
    def sq(i: Int) = i * i
    val sphere = select(-r to r, -r to r, -r to r, sq(_) + sq(_) + sq(_) <= r * r)
    sphere.animate("3D-printed sphere", 3, 0)

  def prepend[A] = augment((a: A, l: Seq[A]) => Seq(a) ++ l)

  def queens(n: Int, k: Int = 0): Seq[Seq[Int]] =
    prepend(0 until n, queens(n, k + 1), isSafe) until k == n

  def isSafe(col: Int, queens: Seq[Int]): Boolean =
    val row = queens.length
    val queensWithRow = (row - 1 to 0 by -1) zip queens
    queensWithRow forall { case (r, c) => col != c && math.abs(col - c) != row - r }

  test("8 queens"):
    val n = 8
    val res = queens(n).toList

    def listFromInd(i: Int, ct: Int) =
      (0 until ct).map(x => if x == i then 1 else 0).toList

    val res1 = res.map(l => l.map(listFromInd(_, n).toArray).toArray).toArray
    val res2 = res.map(l => l.map(listFromInd(_, n)))
    val arr = multiArray(0 until res1.length, 1 to n, (1 to n).map(x => (x + 64).toChar).toList, res2)

    arr.animate("Queens", true, 750, false, "", false, Some(imageSrcs.chessImageSrcs))
