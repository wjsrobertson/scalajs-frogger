package net.xylophones.frogger

import org.scalajs.dom
import org.scalajs.dom.document
import org.scalajs.dom.html.Canvas

import scala.scalajs.js.JSApp
import scala.scalajs.js.timers._

object App extends JSApp {
  val canvas = document.createElement("canvas").asInstanceOf[Canvas]
  val ctx = canvas.getContext("2d").asInstanceOf[dom.CanvasRenderingContext2D]

  def main(): Unit = {
    addCanvas()
    val m = Layers.initialModel()
    loop(model = m)
  }

  def loop(lastTime: Long = System.currentTimeMillis, model: Model): Unit = {
    draw(model)

    val interval: Long = System.currentTimeMillis() - lastTime
    val desiredPause = 1000 / 25
    val pause = Math.max(0, desiredPause - interval)

    setTimeout(pause) {
      val newModel: Model = updateModel(model)

      loop(System.currentTimeMillis(), newModel)
    }
  }

  private def updateModel(model: Model): Model = {
    ModelUpdaters.updaters.foldLeft(model) {
      (updatedModel: Model, updater: ModelUpdater) => updater.update(updatedModel)
    }
  }

  def draw(model: Model) = {
    (model.layers.all zip model.positions).foreach { case (l, p) => l.draw(ctx, p, model) }

    model.layers.frogLayer.draw(ctx, model.frogPosition, model)
    //model.layers.frog.draw(ctx, model.frogPosition, model.frogFacing.frame)
  }

  def addCanvas(): Unit = {
    canvas.width = 512
    canvas.height = (0.95 * dom.window.innerHeight).toInt
    //canvas.style.backgroundColor = "#FF0000"
    document.body.appendChild(canvas)
  }
}
