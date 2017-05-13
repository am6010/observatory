package observatory

import Visualization._

import scala.collection.immutable.IndexedSeq
import scala.collection.parallel.immutable.ParMap

/**
  * 4th milestone: value-added information
  */
object Manipulation {

  val cor: IndexedSeq[Location] = for {
    lat <- -89 to 90 by 1
    lon <- -180 to 179 by 1
  } yield Location(lat, lon)

  /**
    * @param temperatures Known temperatures
    * @return A function that, given a latitude in [-89, 90] and a longitude in [-180, 179],
    *         returns the predicted temperature at this location
    */
  def makeGrid(temperatures: Iterable[(Location, Double)]): (Int, Int) => Double = {

    val grid: ParMap[(Int, Int), Double] = cor.par.foldLeft(ParMap.empty[(Int, Int), Double]) { (map, loc) =>
      map + ((loc.lat.toInt, loc.lon.toInt) -> predictTemperature(temperatures, loc))
    }

    (lat, lon) => grid((lat, lon))
  }

  /**
    * @param temperaturess Sequence of known temperatures over the years (each element of the collection
    *                      is a collection of pairs of location and temperature)
    * @return A function that, given a latitude and a longitude, returns the average temperature at this location
    */
  def average(temperaturess: Iterable[Iterable[(Location, Double)]]): (Int, Int) => Double = {
    val years = temperaturess.size
    val map = temperaturess
      .map(temperatures => makeGrid(temperatures))
      .flatMap(grid => cor.map(x => ((x.lat.toInt, x.lon.toInt), grid(x.lat.toInt, x.lon.toInt))))
      .groupBy(_._1)
      .mapValues(x => x.map(_._2).sum / years)
    (lat, lon) => map((lat, lon))
  }

  /**
    * @param temperatures Known temperatures
    * @param normals A grid containing the “normal” temperatures
    * @return A sequence of grids containing the deviations compared to the normal temperatures
    */
  def deviation(temperatures: Iterable[(Location, Double)], normals: (Int, Int) => Double): (Int, Int) => Double = {
    val grid = makeGrid(temperatures)

    val map = cor.map {
      case Location(lat, lon) =>
      ((lat.toInt, lon.toInt), grid(lat.toInt, lon.toInt) - normals(lat.toInt, lon.toInt))
    }.toMap

    (lat, lon) => map((lat, lon))
  }


}

