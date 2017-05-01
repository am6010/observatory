package observatory


import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import org.scalatest.prop.Checkers

import Visualization._

@RunWith(classOf[JUnitRunner])
class VisualizationTest extends FunSuite with Checkers {
  val colors =
    List((60.0, Color(255,255,255)), (32.0, Color(255, 0, 0)),
      (12.0, Color(255, 255, 0)), (0.0, Color(0, 255, 255)),
      (-15.0, Color(0, 0, 255)), (-27.0, Color(255, 0, 255)),
      (-50.0, Color(33, 0, 107)), (-60.0, Color(0, 0, 0)))


  test("calculate distance") {
    val loc = convertDegreesToRadiant(Location(45.0, 45.0))
    val actual = calculateSphericalDistance(Location(0.0, 0.0), loc)
    val expected =  math.acos(0.5d)
    assert(math.abs(actual -expected) < 0.0001, "Should return the correct distance")
  }

  test("test predictTemperature function with less that 1KM distance") {
    val stationsData = Seq((Location(0.0, 0.0), 10.0), (Location(45.0, 0.0), 15.0))
    val location = Location(0.0, 0.0)
    val actual = predictTemperature(stationsData, location)
    assert(actual === 10.0, "Should be the same temperature as the first station")
  }

  test("test predictTemperature function") {
    val stationsData = Seq((Location(0.0, 0.0), 10.0), (Location(90.0, 0.0), 15.0))
    val location = Location(45.0, 0.0)
    val distances = stationsData.map(x => (Visualization.calculateSphericalDistance(location, x._1), x._2))
    val weights = distances.map(x => (Visualization.calculateWeight(x._1), x._2))
    val expected = weights.map(x => x._1 * x._2).sum / weights.map(_._1).sum
    val actual = predictTemperature(stationsData, location)
    assert(math.abs(actual - expected) < 0.0000001, "Should have the expected temperature")
  }

  test("Sample test") {
    val result = predictTemperature(List((Location(0.0,0.0),10.0)), location=Location(88.0,-174.0))
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
    val result = predictTemperature(list, Location(0, 0))
    assert(math.abs(temp1 - result) < math.abs(temp2 - result))
  }

  test("greatCircleDistance test (extreme case 1)") {
    val loc1 = Visualization.convertDegreesToRadiant(Location(-12.0, 85.0))
    val loc2 = Visualization.convertDegreesToRadiant(Location(12.0, -95.0))
    assert(calculateSphericalDistance(loc1,loc2 ) === math.Pi)
  }

  test("greatCircleDistance test (extreme case 2)") {
    val loc1 = Visualization.convertDegreesToRadiant(Location(90, -180))
    val loc2 = Visualization.convertDegreesToRadiant(Location(0.0, 0.0))
    assert(math.abs(calculateSphericalDistance(loc1,loc2 ) - 1.57079633D) < 0.00001)
  }

  test("Color interpolation 1") {
    val scale = List((0.0, Color(0, 0, 255)))
    assert(interpolateColor(scale, -0.5) == Color(0, 0, 255))
    assert(interpolateColor(scale, 0.5) == Color(0, 0, 255))
    assert(interpolateColor(scale, 0.0) == Color(0, 0, 255))
  }

  test("Color interpolation 2") {
    assert(interpolateColor(colors, 12.0) == Color(255, 255, 0))
  }

  test("Color interpolation 3") {

    assert(interpolateColor(colors, 62) == Color(255, 255, 255))
  }

  test("Color interpolation 4") {
    assert(interpolateColor(colors, 6) == Color(128, 255, 128))
  }

  test("Color interpolation 5") {
    assert(interpolateColor(colors, -7.5) == Color(0, 128, 255))
  }
}
