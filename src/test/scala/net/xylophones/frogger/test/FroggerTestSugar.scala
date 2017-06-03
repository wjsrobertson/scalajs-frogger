package net.xylophones.frogger.test

import net.xylophones.frogger._
import org.mockito.Mockito.when
import org.scalajs.dom.raw.HTMLImageElement
import org.scalatest.mockito.MockitoSugar

trait FroggerTestSugar extends MockitoSugar {

  def createTiledImage(width: Int, height: Int): TiledImage = {
    val htmlImage = mock[HTMLImageElement]
    val image = new Image(htmlImage)
    when(htmlImage.width).thenReturn(20)
    when(htmlImage.height).thenReturn(10)

    new TiledImage(image, width, height)
  }

  def createSprite(width: Int, height: Int): Sprite = {
    val spriteImage = mock[Image]
    when(spriteImage.width).thenReturn(5)
    when(spriteImage.height).thenReturn(5)

    new Sprite(spriteImage, 5)
  }

  def createModel(channel: Channel, channelPosition: Vector): Model = {
    val layers = Layers(
      scoreTitle = mock[BackgroundLayer],
      scoreLayer = mock[BackgroundLayer],
      scoreSpace = mock[BackgroundLayer],
      homePlaceholder = mock[BackgroundLayer],
      frog = mock[Sprite],
      statusLayer = mock[BackgroundLayer],
      timeLayer = mock[BackgroundLayer],
      channels = Seq(channel),
      homes = Seq()
    )

    val pos = layers.all.map(_ => channelPosition)

    Model(layers=layers, positions = pos)
  }
}
