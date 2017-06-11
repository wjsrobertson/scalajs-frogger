package net.xylophones.frogger

import net.xylophones.frogger.test.FroggerTestSugar
import org.junit.runner.RunWith
import org.scalatest.{BeforeAndAfterEach, FunSuite, Matchers}
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class ChannelCollisionCheckerTest extends FunSuite with Matchers with FroggerTestSugar with BeforeAndAfterEach {

  test("ChannelCollisionChecker detects when landing on a cell") {
    val tiledImage = createTiledImage(10, 10)
    val sprite = createSprite(5, 5)
    val channel = new Channel(Array(Array(Tile(0, 0, CellType.Landable))), 0, tiledImage)
    val chPosition = Vector(10, 10)
    val sPosition = Vector(10, 10)

    val result = ChannelCollisionChecker.isLanding(channel, chPosition, sprite, sPosition)

    result shouldBe true
  }

  test("ChannelCollisionChecker detects deadly collisions") {
    val tiledImage = createTiledImage(16, 16)
    val sprite = createSprite(8, 8)
    val channel = new Channel(Array(Array(Tile(0, 0, CellType.Deadly))), 0, tiledImage)
    val chPosition = Vector(10, 10)
    val sPosition = Vector(10, 10)

    val result = ChannelCollisionChecker.isDeadlyCollision(channel, chPosition, sprite, sPosition)

    result shouldBe true
  }

}
