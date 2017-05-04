package observatory

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import org.scalatest.prop.Checkers

import Interaction._

import scala.collection.concurrent.TrieMap

@RunWith(classOf[JUnitRunner])
class InteractionTest extends FunSuite with Checkers {

  test("tileLocation test 1") {
    val actual = tileLocation(zoom = 2, x = 12, y = 12)
    assert(actual === Location(-89.9999827308541D, 900D))
  }
}
