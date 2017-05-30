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
    val (_, _, m) = GameModel.initialModel()
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

  private def updateModel(model: Model) = {
    val channelPositions: Seq[Vector] = (GameModel.layers zip model.positions)
      .filter(_._1.isInstanceOf[Channel])
      .map(_._2)
    val cp = Channel.update(GameModel.channels, channelPositions)
    val ap = GameModel.allPositions(cp)

    val userDirection = UserInput.direction()
    val jumpDirection: Option[FrogFacing.Direction] =
      if (model.frogJumpTimer > 0) Some(model.frogFacing)
      else if (userDirection.isDefined) userDirection.map(_.dir)
      else None

    // use local variable jumpdirection instead, derived from current model and user input
    val (fp, ff, ft) = jumpDirection match {
      case Some(d) =>
        val scale = 8
        val newFrogPosition = model.frogPosition.add(d.vector.scale(scale))
        val newFrogDirection = d
        val newFrogTimer = if (model.frogJumpTimer >= 1) model.frogJumpTimer -1 else 3

        (newFrogPosition, newFrogDirection, newFrogTimer)
      case _ => (model.frogPosition, model.frogFacing, model.frogJumpTimer)
    }

    val newModel = model.copy(positions = ap, frogPosition = fp, frogFacing = ff, frogJumpTimer = ft)

    newModel
  }

  def draw(model: Model) = {
    (GameModel.layers zip model.positions).foreach { case (l, p) => l.draw(ctx, p) }

    GameModel.frog.draw(ctx, model.frogPosition, model.frogFacing.frame)
  }

  def addCanvas(): Unit = {
    canvas.width = 512
    canvas.height = (0.95 * dom.window.innerHeight).toInt
    canvas.style.backgroundColor = "#FF0000"
    document.body.appendChild(canvas)
  }
}
