package observatory

import java.time.LocalDate

import scala.tools.nsc.interpreter.InputStream
import scala.util.Try

/**
  * 1st milestone: data extraction
  */
object Extraction {

  /**
    * @param year             Year number
    * @param stationsFile     Path of the stations resource file to use (e.g. "/stations.csv")
    * @param temperaturesFile Path of the temperatures resource file to use (e.g. "/1975.csv")
    * @return A sequence containing triplets (date, location, temperature)
    */
  def locateTemperatures(year: Int, stationsFile: String, temperaturesFile: String): Iterable[(LocalDate, Location, Double)] = {

    def getStringIterable(inputStream: InputStream): Iterable[Array[String]] = {
      scala.io.Source.fromInputStream(inputStream).getLines().map(_.split(",")).toIterable
    }

    def fahrenheitToCelcius(fahrenheit: Double): Double = {
      (fahrenheit - 32d) / (9d / 5d)
    }
    val stationsStream = getClass.getResourceAsStream(stationsFile)
    val temperatureStream = getClass.getResourceAsStream(temperaturesFile)

    val stationData: Iterable[((String, String), Location)] = for {
       l <- getStringIterable(stationsStream)
       location <- Try(Location(l(2).toDouble, l(3).toDouble)).toOption
    } yield ((l(0), l(1)), location)

    val temperatureData: Iterable[((String, String), LocalDate, Double)] = for {
      l <- getStringIterable(temperatureStream)
      date <- Try(LocalDate.of(year, l(2).toInt, l(3).toInt)).toOption
      temperature <- Try(fahrenheitToCelcius(l(4).toDouble)).toOption
    } yield ((l(0), l(1)), date, temperature)

    val stationsMap = stationData.par
      .foldLeft(Map.empty[(String, String), Location]){(map, value) => map.updated(value._1, value._2)}

    temperatureData.par.filter(x => stationsMap.contains(x._1)).map{ case (key, date, temperature) =>
      val location = stationsMap(key)
      (date, location, temperature)
    }.toVector
  }

  /**
    * @param records A sequence containing triplets (date, location, temperature)
    * @return A sequence containing, for each location, the average temperature over the year.
    */
  def locationYearlyAverageRecords(records: Iterable[(LocalDate, Location, Double)]): Iterable[(Location, Double)] = {
    val groupedData = records.par.foldLeft(Map.empty[Location, (Double, Long)]){ (map, record) =>
      val oldVal = map.getOrElse(record._2, (0D, 0L))
      map.updated(record._2, (oldVal._1 + record._3, oldVal._2 + 1))
    }
    groupedData.mapValues(x => x._1 / x._2)
  }

}
