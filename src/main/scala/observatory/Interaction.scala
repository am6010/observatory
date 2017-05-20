package observatory

import com.sksamuel.scrimage.{Image, Pixel}

/**
  * 3rd milestone: interactive visualization
  */
object Interaction {

  val PIXEL_LEVEL: Int = 7
  val TILE_SIZE: Int = math.round(math.pow(2, PIXEL_LEVEL)).toInt

  /**
    * @param zoom Zoom level
    * @param x X coordinate
    * @param y Y coordinate
    * @return The latitude and longitude of the top-left corner of the tile, as per http://wiki.openstreetmap.org/wiki/Slippy_map_tilenames
    */
  def tileLocation(zoom: Int, x: Int, y: Int): Location = {
    val Pi =  math.Pi
    Location(
      math.atan(math.sinh(Pi - (2 * Pi * y) / math.pow(2, zoom))).toDegrees,
      ((x * 2 * Pi / math.pow(2, zoom)) - Pi).toDegrees
    )
  }

  /**
    * @param temperatures Known temperatures
    * @param colors Color scale
    * @param zoom Zoom level
    * @param x X coordinate
    * @param y Y coordinate
    * @return A 256×256 image showing the contents of the tile defined by `x`, `y` and `zooms`
    */
  def tile(temperatures: Iterable[(Location, Double)], colors: Iterable[(Double, Color)], zoom: Int, x: Int, y: Int): Image = {
    val coordinates = for {
      j <- 0  until TILE_SIZE
      i <- 0  until TILE_SIZE
    } yield (TILE_SIZE * x + i, TILE_SIZE * y + j)

    val pixels = coordinates.toParArray.map{case (z, l)=>
      val location = tileLocation(PIXEL_LEVEL + zoom, z, l)
      val prediction = Visualization.predictTemperature(temperatures, location)
      val color = Visualization.interpolateColor(colors, prediction)
      Pixel(color.red, color.green, color.blue, 127)
    }.toArray

    Image(TILE_SIZE, TILE_SIZE, pixels)
  }

  /**
    * Generates all the tiles for zoom levels 0 to 3 (included), for all the given years.
    * @param yearlyData Sequence of (year, data), where `data` is some data associated with
    *                   `year`. The type of `data` can be anything.
    * @param generateImage Function that generates an image given a year, a zoom level, the x and
    *                      y coordinates of the tile and the data to build the image from
    */
  def generateTiles[Data](
    yearlyData: Iterable[(Int, Data)],
    generateImage: (Int, Int, Int, Int, Data) => Unit
  ): Unit = {
    val tiles = (for {
      zoom <- 0 to 3
      pow = math.pow(2, zoom).toInt
      x <- 0 until pow
      y <- 0 until pow
    } yield (zoom, x, y)).toVector


    for {
      (year, data) <-  yearlyData
      (zoom, x, y) <- tiles.par
    } {
      generateImage(year, zoom, x, y, data)
    }
  }

}
