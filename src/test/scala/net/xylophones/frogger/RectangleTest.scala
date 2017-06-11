package net.xylophones.frogger

import org.junit.runner.RunWith
import org.scalatest.{BeforeAndAfterEach, FunSuite, Matchers}
import org.scalatest.junit.JUnitRunner
import org.scalatest.mockito.MockitoSugar
import org.scalatest.prop.TableDrivenPropertyChecks._

@RunWith(classOf[JUnitRunner])
class RectangleTest extends FunSuite with Matchers with MockitoSugar with BeforeAndAfterEach {

  // TODO - define behaviour for points on border
  test("Rectangle contains points within its bounds") {
    val rect = new Rectangle(5, 5, 10, 10)

    val withinBounds = Table(
      ("rect position", "point"),
      (Vector(0, 0), Vector(6, 6)),
      (Vector(0, 0), Vector(11, 11)),
      (Vector(10, 10), Vector(16, 16))
    )

    forAll(withinBounds) { (pos: Vector, point: Vector) =>
      rect.contains(pos, point) shouldBe true
    }
  }

  test("Rectangle does not contain points outside its bounds") {
    val rect = new Rectangle(5, 5, 10, 10)

    val notWithinBounds = Table(
      ("rect position", "point"),
      (Vector(0, 0), Vector(4, 4)),
      (Vector(0, 0), Vector(16, 16)),
      (Vector(10, 10), Vector(26, 26))
    )

    forAll(notWithinBounds) { (pos: Vector, point: Vector) =>
      rect.contains(pos, point) shouldBe false
    }
  }

  test("Rectangle intersects with other overlapping Rectangles") {
    val rectPos = Vector(0, 0)
    val rect = new Rectangle(5, 5, 10, 10)

    val overlapping = Table(
      ("rect pos", "other rect", "other pos"),
      (Vector(0, 0), new Rectangle(7, 7, 10, 10), Vector(0, 0)),
      (Vector(0, 0), new Rectangle(8, 8, 10, 10), Vector(0, 0)),
      (Vector(0, 0), new Rectangle(14, 14, 10, 10), Vector(0, 0))
    )

    forAll(overlapping) { (pos: Vector, other: Rectangle, otherPos: Vector) =>
      rect.intersects(pos, other, otherPos) shouldBe true
    }
  }

  test("Rectangle does not intersect with other non-overlapping Rectangles") {
    val rect = new Rectangle(5, 5, 10, 10)

    val nonOverlapping = Table(
      ("rect pos", "other rect", "other pos"),
      (Vector(0, 0), new Rectangle(15, 15, 10, 10), Vector(0, 0)),
      (Vector(0, 0), new Rectangle(16, 16, 10, 10), Vector(0, 0)),
      (Vector(10, 10), new Rectangle(28, 28, 10, 10), Vector(0, 0)),
      (Vector(0, 0), new Rectangle(10, 10, 10, 10), Vector(6, 6))
    )

    forAll(nonOverlapping) { (pos: Vector, other: Rectangle, otherPos: Vector) =>
      rect.intersects(pos, other, otherPos) shouldBe false
    }
  }

  test("Rectangle with padding does not intersect with other non-overlapping Rectangles") {
    val rectWithPadding = new Rectangle(5, 5, 10, 10, 1)
    val rectNoPadding = new Rectangle(14, 14, 10, 10)

    val intersects = rectWithPadding.intersects(Vector(0, 0), rectNoPadding, Vector(0, 0))

    intersects shouldBe false
  }

  test("Two rectangles with padding do not intersect") {
    val rectWithPadding = new Rectangle(5, 5, 10, 10, 1)
    val rectNoPadding = new Rectangle(13, 13, 10, 10, 1)

    val intersects = rectWithPadding.intersects(Vector(0, 0), rectNoPadding, Vector(0, 0))

    intersects shouldBe false
  }

  test("Two rectangles with padding do not intersect other way around") {
    val rectWithPadding = new Rectangle(13, 13, 10, 10, 1)
    val rectNoPadding = new Rectangle(5, 5, 10, 10, 1)

    val intersects = rectWithPadding.intersects(Vector(0, 0), rectNoPadding, Vector(0, 0))

    intersects shouldBe false
  }
}
