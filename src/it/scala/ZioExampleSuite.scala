import mappablethirdparty.given
import examples._

class ZioExampleSuite extends munit.FunSuite:

  test("timer"):

    val res1a = getTimerIO(List("3", "10", "100"))(true, true).result()
    val res1b = getTimerIO(List("3", "a"))(true, true).result()
    val res1c = getTimerIO(List("a", "10"))(true, true).result()
    val res1d = getTimerIO(List("0", "10"))(true, true, "").result()

    val res2a = getTimerIO(List("3", "10", "100"))(false, true).result()
    val res2b = getTimerIO(List("3", "a"))(false, true).result()
    val res2c = getTimerIO(List("a", "10"))(false, true).result()
    val res2d = getTimerIO(List("0", "10"))(false, true, "").result()

    val res3a = getTimerIO(List("3", "10", "100"))(false, false).result()
    val res3b = getTimerIO(List("3", "a"))(false, false).result()
    val res3c = getTimerIO(List("a", "10"))(false, false).result()
    val res3d = getTimerIO(List("0", "10"))(false, false, "").result()

    assertEquals(res1a, Left("Invalid number of command-line parameters"))
    assertEquals(res1b, Left("Invalid int for the GAIN parameter"))
    assertEquals(res1c, Left("Invalid int for the MINUTES parameter"))
    assertEquals(res1d, Left("Could not play the audio clip"))

    assertEquals(res2a, Left("Invalid number of command-line parameters"))
    assertEquals(res2b, Left("Invalid int for the GAIN parameter"))
    assertEquals(res2c, Left("Invalid int for the MINUTES parameter"))
    assertEquals(res2d, Left("Could not play the audio clip"))

    assertEquals(res3a, Left("Invalid number of command-line parameters"))
    assertEquals(res3b, Left("Invalid int for the GAIN parameter"))
    assertEquals(res3c, Left("Invalid int for the MINUTES parameter"))
    assertEquals(res3d, Left("Could not play the audio clip"))

    getTimerIO(List("3", "10"))(false, true).result()

  test("account"):

    val prog1 = getAccountIO(true)
    val prog2 = getAccountIO(false)

    println("\nAccount handling (original):")
    prog1.value()
    println("\nAccount handling (revised):")
    prog2.value()
