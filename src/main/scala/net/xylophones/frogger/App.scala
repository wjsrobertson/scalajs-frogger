package net.xylophones.frogger

import org.scalajs.dom
import org.scalajs.dom.document
import org.scalajs.dom.html.Canvas

import scala.scalajs.js.JSApp
import scala.scalajs.js.annotation.JSExportTopLevel

object App extends JSApp {
  val canvas = document.createElement("canvas").asInstanceOf[Canvas]
  val ctx = canvas.getContext("2d").asInstanceOf[dom.CanvasRenderingContext2D]
  val frog = new Image("img/tiles.png")

  def main(): Unit = {
    appendPar(document.body, "Hello World")
  }

  def appendPar(targetNode: dom.Node, text: String): Unit = {
    //window.requestAnimationFrame()

    canvas.width = (0.95 * dom.window.innerWidth).toInt
    canvas.height = (0.95 * dom.window.innerHeight).toInt
    canvas.style.backgroundColor = "#FF0000"
    document.body.appendChild(canvas)

    val parNode = document.createElement("p")
    val textNode = document.createTextNode(text)
    parNode.appendChild(textNode)
    targetNode.appendChild(parNode)
  }

  @JSExportTopLevel("addClickedMessage")
  def addClickedMessage(): Unit = {

    //val tile = new TiledLayer(new TiledImage(frog, 16, 16), 1, 16, Array(Array(0, 3, 5, 7, 1, 0, 3, 5, 7, 1, 0, 3, 5, 7, 1, 0)))
    //tile.draw(ctx)

    //val channels = ChannelFactory.channels(0)
    //channels(0).draw(ctx)

    /*
    val sprite = new Sprite(frog, 16)
    sprite.setFrame(5)
    sprite.paint(ctx)
    */

    /*
    val homes = (0 to 4).map(HomeFactory.create)
    homes.foreach(h => h.draw(ctx))

    val channels = ChannelFactory.channels(0)
    (0 until 5).foreach{ i =>
      channels(i).moveTo(0, (i * 32) + 48)
    }

    val borderTiles = Array((0 until 16).map(x => Tile(0, 0)).toArray)
    val border = new TiledLayer(new TiledImage(new Image("img/tiles.png"), 32, 32), 1, 16, borderTiles)
    border.moveTo(0, 48 + 5 * 32)
    border.draw(ctx)

    (5 until 10).foreach{ i =>
      channels(i).moveTo(0, (i * 32) + 48 + 32)
    }

    channels.foreach(c => c.draw(ctx))

    val border2 = new TiledLayer(new TiledImage(new Image("img/tiles.png"), 32, 32), 1, 16, borderTiles)
    border2.moveTo(0, 48 + 11 * 32)
    border2.draw(ctx)
    */

    GameModel.order.foreach(_.draw(ctx))

    //ctx.drawImage(frog.element, 48, 0, 16, 16, 0, 0, 16, 16)
  }
}
