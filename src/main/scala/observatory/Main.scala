package observatory

import java.io.File

import Extraction._
import Manipulation._
import Visualization2._
import Interaction._

object Main extends App {

  val colors =
    List((60.0, Color(255,255,255)), (32.0, Color(255, 0, 0)),
      (12.0, Color(255, 255, 0)), (0.0, Color(0, 255, 255)),
      (-15.0, Color(0, 0, 255)), (-27.0, Color(255, 0, 255)),
      (-50.0, Color(33, 0, 107)), (-60.0, Color(0, 0, 0)))

  val stations = "/stations.csv"

  val files = (1975 to 1989).map(year => (year, s"/$year.csv"))

  val locationAvgs = files.toStream
    .map(data => (data._1, locateTemperatures(data._1, stations, data._2)))
    .map(data => (data._1, locationYearlyAverageRecords(data._2)))
    .toVector

  println("Normal Data loaded")

  val normalGrid = average(locationAvgs.map(_._2))

  println("Normal grid ok")

  val files2 = (1990 to 2015).map(year => (year, s"/$year.csv"))

  val locationAvgs2 = files2.toStream
    .map(data => (data._1, locateTemperatures(data._1, stations, data._2)))
    .map(data => (data._1, locationYearlyAverageRecords(data._2)))
    .toVector

  println("Deviations Data loaded")

  val grids = locationAvgs.par
    .map(seq => (seq._1, deviation(seq._2, normalGrid)))
    .toVector

  println("Deviations grid ok")


  generateTiles[(Int, Int)=> Double](grids, (year, zoom, x , y, grid) => {
    val image = visualizeGrid(grid, colors, zoom, x, y)
    image.output(new File(s"target/deviations/$year/$zoom/$x-$y.png"))
  })

  println("Images ok")
}
