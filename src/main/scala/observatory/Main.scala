package observatory

import java.io.File
import java.nio.file.Files._
import java.nio.file.{Files, Paths}

import Extraction._
import Manipulation._
import Visualization2._
import Visualization._
import Interaction._

object Main extends App {

  val colors =
    List((60.0, Color(255,255,255)), (32.0, Color(255, 0, 0)),
      (12.0, Color(255, 255, 0)), (0.0, Color(0, 255, 255)),
      (-15.0, Color(0, 0, 255)), (-27.0, Color(255, 0, 255)),
      (-50.0, Color(33, 0, 107)), (-60.0, Color(0, 0, 0)))

  val stations = "/stations.csv"

  val files = (1977 to 1978 ).map(year => (year, s"/$year.csv"))

  val locationAvgs = files.toStream
    .map(data => (data._1, locateTemperatures(data._1, stations, data._2)))
    .map(data => (data._1, locationYearlyAverageRecords(data._2)))
    .toVector

 println("Normal Data loaded")
/*
  val normalGrid = average(locationAvgs.map(_._2))

  println("Normal grid ok")

  val files2 = (1990 to 2015).map(year => (year, s"/$year.csv"))

  val locationAvgs2 = files2.toStream
    .map(data => (data._1, locateTemperatures(data._1, stations, data._2)))
    .map(data => (data._1, locationYearlyAverageRecords(data._2)))
    .toVector

  println("Deviations Data loaded")

  val grids = locationAvgs2.par
    .map { seq =>
        println(seq._1)
        (seq._1, deviation(seq._2, normalGrid))
    }
    .toVector

  println("Deviations grid ok")


  generateTiles[(Int, Int)=> Double](grids, (year, zoom, x , y, grid) => {
    val image = visualizeGrid(grid, colors, zoom, x, y)
    image.output(new File(s"target/temperatures/$year/$zoom/$x-$y.png"))
  })
   */

  generateTiles[Iterable[(Location, Double)]](locationAvgs, (year, zoom, x, y, data) => {
    val path = Paths.get(s"target/temperatures/$year/$zoom/")

    createDirectories(path)
    val file = new File(path.toString + s"/$x-$y.png")

    val start = System.nanoTime()
    println(s"$year $zoom $x $y")
    val image = tile(data, colors, zoom, x, y)
    println(s"$year $zoom $x $y ${(System.nanoTime() - start) / 1000000000}")
    image.output(file)
  })

  println("Images ok")
}
