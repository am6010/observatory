package observatory

import java.time.LocalDate

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class ExtractionTest extends FunSuite {

  val expected = Seq(
    (LocalDate.of(2015, 8, 11), Location(37.35, -78.433), 27.3),
    (LocalDate.of(2015, 12, 6), Location(37.358, -78.438), 0.0),
    (LocalDate.of(2015, 1, 29), Location(37.358, -78.438), 2.0)
  )

  val limit = 0.000001

  test("Extraction test") {
    val actual = Extraction.locateTemperatures(2015, "/stations-test.csv", "/2015-test.csv")
    assert(actual.size === expected.size, "Should return the expected value for 1975")
    assert(actual.toSeq.map(_._1) === expected.map(_._1))
    assert(actual.toSeq.map(_._2) === expected.map(_._2))
    assert(actual.map(_._3).zip(expected.map(_._3)).forall(x => math.abs(x._1 - x._2) < limit))
  }

  test("locationYearlyAverageRecords test") {
    val actual = Extraction.locationYearlyAverageRecords(expected)
    val expectedAvgs = Seq(
      (Location(37.35, -78.433), 27.3),
      (Location(37.358, -78.438), 1.0)
    )
    assert(actual.size === expectedAvgs.size)
    assert(actual.toSeq.map(_._1) === expectedAvgs.map(_._1))
    assert(actual.map(_._2).zip(expectedAvgs.map(_._2)).forall(x => math.abs(x._1 - x._2) < limit))
  }

  test("1975 test") {
    val records = Extraction.locateTemperatures(1975, "/stations.csv", "/1975.csv")
    val avgs = Extraction.locationYearlyAverageRecords(records)
    assert(avgs.nonEmpty)
  }
}