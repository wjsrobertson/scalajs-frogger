package net.xylophones.frogger


import net.xylophones.frogger.test.FroggerTestSugar
import org.junit.runner.RunWith
import org.scalatest.{BeforeAndAfterEach, FunSuite, Matchers}
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class FrogCollisionCheckerTest extends FunSuite with Matchers with FroggerTestSugar {

  test("FrogDeathChecker sets frog death timer when frog intersects with deadly cell") {
    val frogSprite = createSprite(5, 5)
    val channel = new Channel(Array(Array(Tile(0, 0, CellType.Deadly))), 0, createTiledImage(10, 10))
    val chPosition = Vector(10, 10)
    val fPosition = Vector(10, 10)

    val model = createModel(channel, chPosition)
    val newLayers = model.layers.copy(frog = frogSprite)
    val newModel = model.copy(frogDeathTimer = 0, layers = newLayers, frogPosition = fPosition)

    // when
    val updatedModel = FrogCollisionChecker.update(newModel)

    // then
    updatedModel.frogDeathTimer shouldBe Config.frogDeathTime
  }

}
