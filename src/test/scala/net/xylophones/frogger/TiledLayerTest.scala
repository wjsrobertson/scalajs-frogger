package net.xylophones.frogger

import org.junit.runner.RunWith
import org.scalajs.dom.raw.HTMLImageElement
import org.scalatest.junit.JUnitRunner
import org.scalatest.mockito.MockitoSugar
import org.scalatest.{BeforeAndAfterEach, FunSuite, Matchers}
import org.mockito.Mockito._

@RunWith(classOf[JUnitRunner])
class TiledLayerTest extends FunSuite with Matchers with MockitoSugar with BeforeAndAfterEach {

  val htmlImage = mock[HTMLImageElement]
  val image = new Image(htmlImage)
  when(htmlImage.width).thenReturn(20)
  when(htmlImage.height).thenReturn(10)
  val tiledImage = new TiledImage(image, 10, 10)
  val tiles = Array(Array(Tile(0, 0, 0), Tile(1, 0, 1)))

  val underTest = new TiledLayer(tiledImage, 1, 2, tiles)

  test("TiledLayer contains Rectangles with correct local offset") {
    val rects = underTest.rectangles

    rects.size shouldBe 2
    rects.head.x shouldBe 0
    rects.head.y shouldBe 0
    rects.last.x shouldBe 10
    rects.last.y shouldBe 0
  }
}
