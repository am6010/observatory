package observatory


import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import org.scalatest.prop.Checkers

@RunWith(classOf[JUnitRunner])
class VisualizationTest extends FunSuite with Checkers {


  test("calculate distance") {
    val loc = Visualization.convertDegreesToRadiant(Location(45.0, 45.0))
    val actual = Visualization.calculateSphericalDistance(Location(0.0, 0.0), loc)
    val expected =  math.acos(0.5d)
    assert(math.abs(actual -expected) < 0.0001, "Should return the correct distance")
  }

  test("test predictTemperature function with less that 1KM distance") {
    val stationsData = Seq((Location(0.0, 0.0), 10.0), (Location(45.0, 0.0), 15.0))
    val location = Location(0.0, 0.0)
    val actual = Visualization.predictTemperature(stationsData, location)
    assert(actual === 10.0, "Should be the same temperature as the first station")
  }

  test("test predictTemperature function") {
    val stationsData = Seq((Location(0.0, 0.0), 10.0), (Location(90.0, 0.0), 15.0))
    val location = Location(45.0, 0.0)
    val distances = stationsData.map(x => (Visualization.calculateSphericalDistance(location, x._1), x._2))
    val weights = distances.map(x => (Visualization.calculateWeight(x._1), x._2))
    val expected = weights.map(x => x._1 * x._2).sum / weights.map(_._1).sum
    val actual = Visualization.predictTemperature(stationsData, location)
    assert(math.abs(actual - expected) < 0.0000001, "Should have the expected temperature")
  }

  test("Sample test") {
    val result = Visualization.predictTemperature(List((Location(0.0,0.0),10.0)), location=Location(88.0,-174.0))
    assert(result === 10.0)
  }

  test("predictTemperature: some point closer") {
    val location1 = Location(1,1)
    val temp1 = 10d
    val location2 = Location(-10,-10)
    val temp2 = 50d
    val list = List(
      (location1, temp1),
      (location2, temp2)
    )
    val result = Visualization.predictTemperature(list, Location(0, 0))
    assert(math.abs(temp1 - result) < math.abs(temp2 - result))
  }

  test("greatCircleDistance test (extreme case 1)") {
    val loc1 = Visualization.convertDegreesToRadiant(Location(-12.0, 85.0))
    val loc2 = Visualization.convertDegreesToRadiant(Location(12.0, -95.0))
    assert(Visualization.calculateSphericalDistance(loc1,loc2 ) === math.Pi)
  }

  test("greatCircleDistance test (extreme case 2)") {
    val loc1 = Visualization.convertDegreesToRadiant(Location(90, -180))
    val loc2 = Visualization.convertDegreesToRadiant(Location(0.0, 0.0))
    assert(math.abs(Visualization.calculateSphericalDistance(loc1,loc2 ) - 1.57079633D) < 0.00001)
  }
}
