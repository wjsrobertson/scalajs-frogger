package net.xylophones.frogger

import org.scalajs.dom
import org.scalajs.dom.document
import org.scalajs.dom.html.Canvas

import scala.scalajs.js.JSApp
import scala.scalajs.js.annotation.JSExportTopLevel
import scala.scalajs.js.Dynamic.global
import scala.scalajs.js.timers._

object App extends JSApp {
  val canvas = document.createElement("canvas").asInstanceOf[Canvas]
  val ctx = canvas.getContext("2d").asInstanceOf[dom.CanvasRenderingContext2D]

  def main(): Unit = {
    addCanvas()
    val (layers, p, m) = GameModel.initialModel()
    loop(model=m)
  }

  def loop(lastTime: Long = System.currentTimeMillis, model: Model): Unit = {
    println("loop")

    draw(model.positions)

    val interval: Long = System.currentTimeMillis() - lastTime
    val desiredPause = 1000 / 25
    val pause = Math.max(0, desiredPause - interval)

    setTimeout(pause) {
      val channelPositions: Seq[Vector] = (GameModel.layers zip model.positions)
        .filter(_._1.isInstanceOf[Channel])
        .map(_._2)
      val cp = Channel.update(GameModel.channels, channelPositions)
      val ap = GameModel.allPositions(cp)
      val newModel = model.copy(positions = ap)

      loop(System.currentTimeMillis(), newModel)
    }
  }

  def draw(positions: Seq[Vector]) = {
    (GameModel.layers zip positions).foreach{ case (l, p) => l.draw(ctx, p)}
  }

  def addCanvas(): Unit = {
    canvas.width = (0.95 * dom.window.innerWidth).toInt
    canvas.height = (0.95 * dom.window.innerHeight).toInt
    canvas.style.backgroundColor = "#FF0000"
    document.body.appendChild(canvas)
  }
}
