package observatory

import com.sksamuel.scrimage.{Image, Pixel}

/**
  * 2nd milestone: basic visualization
  */
object Visualization {

  val P: Int = 3

  /**
    * Convert A location angle to radians
    *
    * @param location Input location
    * @return a converted location
    */
  def convertDegreesToRadiant(location: Location): Location = {
    Location(location.lat.toRadians, location.lon.toRadians)
  }

  /**
    * @param loc1 The coordinates of the first location
    * @param loc2 The coordinates of the second location
    * @return The Spherical distance between both input points
    */
  def calculateSphericalDistance(loc1: Location, loc2: Location): Double = {
    val arcCosInput = math.sin(loc1.lat) * math.sin(loc2.lat) +
      math.cos(loc1.lat) * math.cos(loc2.lat) * math.cos(math.abs(loc1.lon - loc2.lon))

    arcCosInput match {
      case _ if arcCosInput >= 1 => math.acos(1)
      case _ if arcCosInput <= -1 => math.acos(-1)
      case _ => math.acos(arcCosInput)
    }
  }

  /**
    * @param distance the distance from the target location
    * @param p        indicates the power of the denominator
    * @return the weight for the Inverse distance weighting formula
    */
  def calculateWeight(distance: Double, p: Int = P): Double = {
    1 / math.pow(distance, p)
  }

  /**
    * @param temperatures Known temperatures: pairs containing a location and the temperature at this location
    * @param location     Location where to predict the temperature
    * @return The predicted temperature at `location`
    */
  def predictTemperature(temperatures: Iterable[(Location, Double)], location: Location): Double = {

    val convertedLocation = convertDegreesToRadiant(location)

    val distances = temperatures.par.map { case (loc, temp) =>
      (calculateSphericalDistance(convertedLocation, convertDegreesToRadiant(loc)), temp)
    }

    val lessThanOneKM = distances.find(_._1 <= 0.001)

    if (lessThanOneKM.isDefined) {
      lessThanOneKM.get._2
    } else {
      val weights = distances.map { case (dist, temp) => (calculateWeight(dist), temp) }
      val (numerator, denominator) = weights.foldLeft((0d, 0d))((s, e) => (s._1 + e._1 * e._2, s._2 + e._1))
      numerator / denominator
    }
  }

  /**
    * @param points Pairs containing a value and its associated color
    * @param value  The value to interpolate
    * @return The color that corresponds to `value`, according to the color scale defined by `points`
    */
  def interpolateColor(points: Iterable[(Double, Color)], value: Double): Color = {

    val max = points.maxBy(_._1)
    val min = points.minBy(_._1)

    def keep2ClosestPoints(seq: Seq[(Double, Color)], point: (Double, Color)): Seq[(Double, Color)] = {
      if (seq.isEmpty || seq.size < 2) {
        seq :+ point
      } else {
        val dif = math.abs(point._1 - value)
        val max = seq.maxBy(x => math.abs(x._1 - value))

        if (math.abs(max._1 - value) > dif) {
          seq.filter(x => x != max) :+ point
        } else {
          seq
        }
      }
    }

    def unWrap2Points(twoPoints: Seq[(Double, Color)]): Color = {
      twoPoints match {
        case h :: Nil => h._2
        case first :: second :: Nil =>
          value match {
            case _ if first._1 - value == 0 => first._2
            case _ if second._1 - value == 0 => second._2
            case _ =>
              val xDif = second._1 - first._1
              Color(
                math.round(first._2.red + (value - first._1) * (second._2.red - first._2.red) / xDif).toInt,
                math.round(first._2.green + (value - first._1) * (second._2.green - first._2.green) / xDif).toInt,
                math.round(first._2.blue + (value - first._1) * (second._2.blue - first._2.blue) / xDif).toInt
              )
          }
        case _ => throw new Error("Sequence should have only 2 elements!")
      }
    }

    value match {
      case _ if value > max._1 => max._2
      case _ if value < min._1 => min._2
      case _ =>
        val twoPoints = points.foldLeft(Seq[(Double, Color)]()) (keep2ClosestPoints)
        unWrap2Points(twoPoints)
    }
  }

  /**
    * @param temperatures Known temperatures
    * @param colors       Color scale
    * @return A 360×180 image where each pixel shows the predicted temperature at its location
    */
  def visualize(temperatures: Iterable[(Location, Double)], colors: Iterable[(Double, Color)]): Image = {

    val colorsVector: Stream[Location] = (for {
      lat <- 90 to -89 by -1
      lon <- -180 to 179
    } yield Location(lat, lon)).toStream

    val pixels = colorsVector.par
      .map(loc => predictTemperature(temperatures, loc))
      .map(temperature => interpolateColor(colors, temperature))
      .map(color => Pixel(color.red, color.green, color.blue, 255))
      .toArray
    Image(360, 180, pixels = pixels)
  }

}

