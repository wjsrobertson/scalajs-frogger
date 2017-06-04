package net.xylophones.frogger
import net.xylophones.frogger.App.ctx
import net.xylophones.frogger.Layers.darkBlue
import org.scalajs.dom.CanvasRenderingContext2D

class ScoreTitleLayer extends Layer(Config.gameWidth, Config.scoreTitleHeight) {
  val oneUp = Image("img/1_up.png")
  val highScore = Image("img/high_score.png")

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
  val nums = Image("img/numbers.png")
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

      digits.reverse.zipWithIndex.foreach{ ni =>
        val digit = ni._1.toInt
        val sequence = ni._2
        val sourceX = digit * tileSize
        val writeX =  (endX-tileSize) - sequence * tileSize

        context.drawImage(nums.element, sourceX, 0, tileSize, tileSize, writeX, y, tileSize, tileSize)
    }
  }
}

// TODO - can I use a shorter version of drawImage ?
class StatusLayer extends Layer(Config.gameWidth, Config.statusHeight) {
  val lifeImage = Image("img/life.png")
  val levelImage = Image("img/level2.png")

  override def draw(context: CanvasRenderingContext2D, position: Vector, model: Model): Unit = {
    context.fillStyle = "#000000"
    context.strokeStyle = "#000000"
    context.fillRect(position.x, position.y, width, height)

    (0 until model.lives).foreach{ life =>
      val x = life * lifeImage.width
      context.drawImage(lifeImage.element, 0, 0, lifeImage.width, lifeImage.height, x + position.x, position.y, lifeImage.width, lifeImage.height)
    }

    (0 until model.level).foreach{ level =>
      val x = width - level * levelImage.width
      context.drawImage(levelImage.element, 0, 0, levelImage.width, levelImage.height, x + position.x, position.y, levelImage.width, levelImage.height)
    }
  }
}

class TimeLayer extends Layer(Config.gameWidth, Config.timeHeight) {
  private val timeImage = Image("img/time.png")

  // TODO - clean this garbage up
  override def draw(context: CanvasRenderingContext2D, position: Vector, model: Model): Unit = {
    val imageX = Config.gameWidth - Config.timeImageWidth

    val usedRatio: Double = model.levelDurationMs().toDouble / Config.levelTimeLimitMs.toDouble
    val remainingRatio: Double = 1 - usedRatio
    val fullLength = imageX

    val start = fullLength.toDouble * usedRatio
    val w = fullLength.toDouble * remainingRatio.toDouble

    context.fillStyle = "#000000"
    context.strokeStyle = "#000000"
    context.fillRect(position.x, position.y, Config.gameWidth, Config.timeHeight)

    context.fillStyle = "#00FF00"
    context.strokeStyle = "#00FF00"
    context.fillRect(start, position.y, w, Config.timeHeight)

    context.drawImage(timeImage.element, 0, 0, timeImage.width, timeImage.height, imageX, position.y, timeImage.width, timeImage.height)
  }
}

class FrogLayer(val frog: Sprite, val deadFrog: Sprite) extends Layer(Config.frogWidth, Config.frogHeight) {
  override def draw(context: CanvasRenderingContext2D, position: Vector, model: Model): Unit =
    if (model.inDeathAnimation()) {
      val ratio = model.frogDeathTimer.toDouble / Config.frogDeathTime.toDouble
      val frame = Math.min((ratio * 4).floor, 3).toInt
      deadFrog.draw(ctx, model.frogPosition, frame)
    } else if (model.inPlay()) {
      frog.draw(ctx, model.frogPosition, model.frogFacing.frame)
    }
}
