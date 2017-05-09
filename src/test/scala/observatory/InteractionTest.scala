package observatory

import java.io.File

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import org.scalatest.prop.Checkers
import Interaction._
import observatory.Visualization.visualize

import scala.collection.concurrent.TrieMap

@RunWith(classOf[JUnitRunner])
class InteractionTest extends FunSuite with Checkers {

  val colors = List(
    (-60.0, Color(0, 0, 0)),
    (-50.0, Color(33, 0, 107)),
    (-27.0, Color(255, 0, 255)),
    (-15.0, Color(0, 0, 255)),
    (0.0, Color(0, 255, 255)),
    (12.0, Color(255, 255, 0)),
    (32.0, Color(255, 0, 0)),
    (60.0, Color(255,255,255))
  )

  test("tileLocation test 1") {
    val actual = tileLocation(zoom = 2, x = 12, y = 12)
    assert(actual === Location(-89.9999827308541D, 900D))
  }

  test("tile test 1") {
    val temperatures1 = Array(
      (Location(45, -90), 0.0),
      (Location(45, 90), 32.0),
      (Location(-45, 90), 32.0),
      (Location(-45, -90), 0.0)
    )
    val image = tile(temperatures1, colors, 0, 0 , 0)
    val result = image.output(new File("target/test.png"))
    assert(result !== null)
  }

  test("visualize image from 1975") {
    val records = Extraction.locateTemperatures(1975, "/stations.csv", "/1975.csv")
    val avgs = Extraction.locationYearlyAverageRecords(records)
    val image = tile(avgs, colors, 1, 1, 0)
    val result = image.output(new File("target/1975_2.png"))
    assert(result !== null)
  }
}
