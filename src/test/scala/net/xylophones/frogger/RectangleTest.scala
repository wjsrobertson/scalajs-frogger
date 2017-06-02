package net.xylophones.frogger

import org.junit.runner.RunWith
import org.scalatest.{BeforeAndAfterEach, FunSuite, Matchers}
import org.scalatest.junit.JUnitRunner
import org.scalatest.mockito.MockitoSugar
import org.scalatest.prop.TableDrivenPropertyChecks._

@RunWith(classOf[JUnitRunner])
class RectangleTest extends FunSuite with Matchers with MockitoSugar with BeforeAndAfterEach {

  test("Rectangle contains points within its bounds") {
    val rect = new Rectangle(5, 5, 10, 10)

    val contained = Table(
      ("rect position", "point"),
      (Vector(0, 0), Vector(6, 6)),
      (Vector(0, 0), Vector(11, 11)),
      (Vector(10, 10), Vector(16, 16))
    )

    forAll(contained) { (pos: Vector, point: Vector) =>
      rect.contains(pos, point) shouldBe true
    }
  }

  test("Rectangle does not contain points outside its bounds") {
    val rect = new Rectangle(5, 5, 10, 10)

    val contained = Table(
      ("rect position", "point"),
      (Vector(0, 0), Vector(4, 4)),
      (Vector(0, 0), Vector(16, 16)),
      (Vector(10, 10), Vector(26, 26))
    )

    forAll(contained) { (pos: Vector, point: Vector) =>
      rect.contains(pos, point) shouldBe false
    }
  }
}
