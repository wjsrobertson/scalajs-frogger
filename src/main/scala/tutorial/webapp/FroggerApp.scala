package tutorial.webapp

import scala.scalajs.js.JSApp
import org.scalajs.dom
import dom.document
import dom.window
import org.scalajs.dom.html.Canvas
import org.scalajs.dom.raw.HTMLImageElement
import net.xylophones.frogger._

import scala.scalajs.js.annotation.JSExportTopLevel

object FroggerApp extends JSApp {
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

    val home = HomeFactory.create(1)
    home.draw(ctx)

    //ctx.drawImage(frog.element, 48, 0, 16, 16, 0, 0, 16, 16)
  }
}
