import augmented.given
import typeconstpm._
import typeconstpm.given
import typeconstpm.Extensions._

import com.google.common.collect._
import scala.jdk.CollectionConverters.*

class TypeConstructorPmSuite extends munit.FunSuite:

  val cityTable: Table[String, String, String] = HashBasedTable.create()
  cityTable.put("Argentina", "capital", "Buenos Aires")
  cityTable.put("Argentina", "largest city", "Buenos Aires")
  cityTable.put("Bolivia", "capital", "La Paz")
  cityTable.put("Bolivia", "co-capital", "Sucre")
  cityTable.put("Brazil", "capital", "Brasilia")
  cityTable.put("Brazil", "largest city", "Sao Paulo")

  val cityTreeTable: TreeBasedTable[String, String, String] = TreeBasedTable.create()
  cityTreeTable.put("Argentina", "capital", "Buenos Aires")
  cityTreeTable.put("Argentina", "largest city", "Buenos Aires")
  cityTreeTable.put("Bolivia", "capital", "La Paz")
  cityTreeTable.put("Bolivia", "co-capital", "Sucre")
  cityTreeTable.put("Brazil", "capital", "Brasilia")
  cityTreeTable.put("Brazil", "largest city", "Sao Paulo")

  test("cities"):

    def getInfo = (country: String, cityType: String, cityName: String) => f"In $country, the $cityType is $cityName."

    val cityInfo = getInfo(cityTable)
    val cityTreeInfo = getInfo(cityTreeTable)

    assertEquals(cityInfo.head, "In Argentina, the capital is Buenos Aires.")
    assertEquals(cityInfo.last, "In Brazil, the largest city is Sao Paulo.")
    assertEquals(cityTreeInfo.head, "In Argentina, the capital is Buenos Aires.")
    assertEquals(cityTreeInfo.last, "In Brazil, the largest city is Sao Paulo.")

  test("distance"):

    def distance = (x: Double, y: Double) => Math.sqrt(x * x + y * y)

    val namedPair1 = NamedPair("Origin", 0.0, 0.0)
    val namedPair2 = NamedPair("Vertex", 3.0, 4.0)

    val dist1 = distance(namedPair1)
    val dist2 = distance(namedPair2)
    assertEquals(dist1, 0.0)
    assertEquals(dist2, 5.0)

  test("multimap"):

    def square = (x: Int) => f"Square: ${x * x}"

    val multimap: ListMultimap[String, Int] = MultimapBuilder.treeKeys().arrayListValues().build()

    multimap.get("a").add(12)
    multimap.get("a").add(10)
    multimap.get("b").add(5)
    multimap.get("c").add(20)
    multimap.get("c").add(8)

    val multimap1 = square(multimap)

    assertEquals(multimap1.get("a").asScala.head, "Square: 144")
    assertEquals(multimap1.get("a").asScala.last, "Square: 100")
    assertEquals(multimap1.get("c").asScala.head, "Square: 400")
    assertEquals(multimap1.get("c").asScala.last, "Square: 64")
