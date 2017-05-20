package observatory

import com.sksamuel.scrimage.{Image, Pixel}
import Interaction._
import Visualization._

/**
  * 5th milestone: value-added information visualization
  */
object Visualization2 {

  /**
    * @param x X coordinate between 0 and 1
    * @param y Y coordinate between 0 and 1
    * @param d00 Top-left value
    * @param d01 Bottom-left value
    * @param d10 Top-right value
    * @param d11 Bottom-right value
    * @return A guess of the value at (x, y) based on the four known values, using bilinear interpolation
    *         See https://en.wikipedia.org/wiki/Bilinear_interpolation#Unit_Square
    */
  def bilinearInterpolation(
    x: Double,
    y: Double,
    d00: Double,
    d01: Double,
    d10: Double,
    d11: Double
  ): Double = {
    (d00 * (1 - x) * (1-y)) + (d10 * x * (1-y))+
      (d01 * (1-x) * y) + (d11 * x * y)
  }

  /**
    * @param grid Grid to visualize
    * @param colors Color scale to use
    * @param zoom Zoom level of the tile to visualize
    * @param x X value of the tile to visualize
    * @param y Y value of the tile to visualize
    * @return The image of the tile at (x, y, zoom) showing the grid using the given color scale
    */
  def visualizeGrid(
    grid: (Int, Int) => Double,
    colors: Iterable[(Double, Color)],
    zoom: Int,
    x: Int,
    y: Int
  ): Image = {

    def normalizeBounds(value: Int, min: Int, max: Int): Int = {
      math.max(math.min(value, max), min)
    }

    val coordinates = for {
      j <- 0  until TILE_SIZE
      i <- 0  until TILE_SIZE
    } yield (TILE_SIZE * x + i, TILE_SIZE * y + j)

    val pixels = coordinates.par.map { case (z, l) =>
      val location = tileLocation(zoom + 8, z, l)
      val floorLat = normalizeBounds(math.floor(location.lat).toInt, -88, 89)
      val floorLon = normalizeBounds(math.floor(location.lon).toInt, -179, 178)
      val prediction = bilinearInterpolation(
        location.lat - floorLat,
        location.lon - floorLon,
        grid(floorLat, floorLon),
        grid(floorLat, floorLon + 1),
        grid(floorLat + 1, floorLon),
        grid(floorLat + 1, floorLon + 1)
      )
      val color = interpolateColor(colors, prediction)
      Pixel(color.red, color.green, color.blue, 127)
    }

    Image(TILE_SIZE, TILE_SIZE, pixels.toArray)
  }

}
