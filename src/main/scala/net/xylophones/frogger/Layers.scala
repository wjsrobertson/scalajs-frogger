package net.xylophones.frogger

import net.xylophones.frogger.App.ctx
import org.scalajs.dom.CanvasRenderingContext2D
import net.xylophones.frogger.Images._

class ScoreTitleLayer extends Layer(Config.gameWidth, Config.scoreTitleHeight) {

  // TODO - don't use magic variables here
  override def draw(context: CanvasRenderingContext2D, position: Vector, model: Model): Unit = {
    context.fillStyle = "#00002A"
    context.strokeStyle = "#00002A"
    context.fillRect(position.x, position.y, Config.gameWidth, Config.scoreTitleHeight)

    context.drawImage(oneUp.element, 0, 0, oneUp.width, oneUp.height, 68, position.y, oneUp.width, oneUp.height)

    context.drawImage(highScore.element, 0, 0, highScore.width, highScore.height, 193, position.y, highScore.width, highScore.height)
  }
}

class ScoreLayer extends Layer(Config.gameWidth, Config.scoreHeight) {
  val tileSize = 16
  // TODO - don't use magic variables here

  override def draw(context: CanvasRenderingContext2D, position: Vector, model: Model): Unit = {
    context.fillStyle = "#00002A"
    context.strokeStyle = "#00002A"
    context.fillRect(position.x, position.y, Config.gameWidth, Config.scoreTitleHeight)

    drawNumber(context, 128, position.y, model.score)
    drawNumber(context, 320, position.y, model.highScore)
  }

  def drawNumber(context: CanvasRenderingContext2D, endX: Int, y: Int, numtoDraw: Int): Unit = {
    val digits = numtoDraw.toString.split("")

    digits.reverse.zipWithIndex.foreach { ni =>
      val digit = ni._1.toInt
      val sequence = ni._2
      val sourceX = digit * tileSize
      val writeX = (endX - tileSize) - sequence * tileSize

      context.drawImage(nums.element, sourceX, 0, tileSize, tileSize, writeX, y, tileSize, tileSize)
    }
  }
}

// TODO - can I use a shorter version of drawImage ?
class StatusLayer extends Layer(Config.gameWidth, Config.statusHeight) {
  override def draw(context: CanvasRenderingContext2D, position: Vector, model: Model): Unit = {
    context.fillStyle = "#000000"
    context.strokeStyle = "#000000"
    context.fillRect(position.x, position.y, width, height)

    (0 until model.lives).foreach { life =>
      val x = life * lifeImage.width
      context.drawImage(lifeImage.element, 0, 0, lifeImage.width, lifeImage.height, x + position.x, position.y, lifeImage.width, lifeImage.height)
    }

    (0 until model.level).foreach { level =>
      val x = width - (level + 1) * levelImage.width
      context.drawImage(levelImage.element, 0, 0, levelImage.width, levelImage.height, x + position.x, position.y, levelImage.width, levelImage.height)
    }
  }
}

class TimeLayer extends Layer(Config.gameWidth, Config.timeHeight) {
  // TODO - clean this garbage up
  override def draw(context: CanvasRenderingContext2D, position: Vector, model: Model): Unit = {
    context.fillStyle = "#000000"
    context.strokeStyle = "#000000"
    context.fillRect(position.x, position.y, Config.gameWidth, Config.timeHeight)

    val imageX = Config.gameWidth - Config.timeImageWidth

    if (model.playState == PlayState.InPlay) {
      val usedRatio: Double = model.levelDurationMs().toDouble / Config.levelTimeLimitMs.toDouble
      val remainingRatio: Double = 1 - usedRatio
      val fullLength = imageX

      val start = fullLength.toDouble * usedRatio
      val w = fullLength.toDouble * remainingRatio.toDouble

      if (model.lowOnTime) {
        context.fillStyle = "#FF0000"
        context.strokeStyle = "#FF0000"
        context.fillRect(start, position.y, w, Config.timeHeight)
      } else {
        context.fillStyle = "#00FF00"
        context.strokeStyle = "#00FF00"
        context.fillRect(start, position.y, w, Config.timeHeight)
      }
    }

    context.drawImage(timeImage.element, 0, 0, timeImage.width, timeImage.height, imageX, position.y, timeImage.width, timeImage.height)
  }
}

class FrogLayer(val frog: Sprite, val deadFrog: Sprite) extends Layer(Config.frogWidth, Config.frogHeight) {
  override def draw(context: CanvasRenderingContext2D, position: Vector, model: Model): Unit =
    if (model.playState == PlayState.FrogDeathAnimation) {
      val ratio = model.frogDeathTimer.toDouble / Config.frogDeathTime.toDouble
      val frame = 3 - Math.min((ratio * 4).floor, 3).toInt
      deadFrog.draw(ctx, model.frogPosition, frame)
    } else if (model.playState == PlayState.InPlay) {
      frog.draw(ctx, model.frogPosition, model.frogFacing.frame)
    }
}

class BackgroundLayer(val w: Int, val h: Int, colour: String) extends Layer(w, h) {
  override def draw(context: CanvasRenderingContext2D, position: Vector, model: Model) = {
    context.fillStyle = colour
    context.strokeStyle = colour
    context.fillRect(position.x, position.y, width, height)
  }
}
