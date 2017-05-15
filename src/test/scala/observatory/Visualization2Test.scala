package observatory

import java.io.File

import observatory.Manipulation._
import observatory.Interaction.tile
import observatory.Visualization2._
import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import org.scalatest.prop.Checkers

@RunWith(classOf[JUnitRunner])
class Visualization2Test extends FunSuite with Checkers {

  val colors =
    List((60.0, Color(255,255,255)), (32.0, Color(255, 0, 0)),
      (12.0, Color(255, 255, 0)), (0.0, Color(0, 255, 255)),
      (-15.0, Color(0, 0, 255)), (-27.0, Color(255, 0, 255)),
      (-50.0, Color(33, 0, 107)), (-60.0, Color(0, 0, 0)))


  test("visualize image from 1975") {
    val records = Extraction.locateTemperatures(1975, "/stations.csv", "/1975.csv")
    val avgs = Extraction.locationYearlyAverageRecords(records)
    val grid = makeGrid(avgs)
    val image = visualizeGrid(grid, colors, 0, 0 ,0)
    val result = image.output(new File("target/1975_3.png"))
    assert(result !== null)
  }
}
