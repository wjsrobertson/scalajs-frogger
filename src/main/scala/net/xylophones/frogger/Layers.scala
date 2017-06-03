package net.xylophones.frogger
import net.xylophones.frogger.Layers.darkBlue
import org.scalajs.dom.CanvasRenderingContext2D

object Direction {

  abstract sealed class Dir(val frame: Int, val vector: Vector)

  case object Up extends Dir(0, Vector(0, -1))

  case object Down extends Dir(1, Vector(0, 1))

  case object Left extends Dir(2, Vector(-1, 0))

  case object Right extends Dir(3, Vector(1, 0))
}

case class Model(score: Int = 0,
                 highScore: Int = 0,
                 lives: Int = 0,
                 frogState: Int = 0,
                 level: Int = 1,
                 levelStartTimeMs: Long = System.currentTimeMillis(),
                 frogJumpTimer: Int = 0,
                 frogPosition: Vector = Vector(0, 0),
                 frogFacing: Direction.Dir = Direction.Up,
                 positions: Seq[Vector],
                 layers: Layers,
                 frogDeathTimer: Int = 0) {

  def levelDurationMs() = System.currentTimeMillis() - levelStartTimeMs

  def channelsWithPositions() = {
    val channelsPos: Seq[(Vector)] = (layers.all zip positions)
      .filter(_._1.isInstanceOf[Channel])
      .map(_._2)

    layers.channels zip channelsPos
  }
}

case class Layers(scoreTitle: ScoreTitleLayer,
                  scoreLayer: ScoreLayer,
                  scoreSpace: BackgroundLayer,
                  homePlaceholder: BackgroundLayer,
                  frog: Sprite,
                  statusLayer: StatusLayer,
                  timeLayer: TimeLayer,
                  channels: Seq[Channel],
                  homes: Seq[Home]) {

  def all: Seq[Layer] =
    Seq(scoreTitle, scoreLayer, scoreSpace, homePlaceholder) ++
      channels ++
      Seq(statusLayer, timeLayer) ++
      homes
}

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

    (0 to model.lives).foreach{ life =>
      val x = life * lifeImage.width
      context.drawImage(lifeImage.element, 0, 0, lifeImage.width, lifeImage.height, x + position.x, position.y, lifeImage.width, lifeImage.height)
    }

    (0 to model.level).foreach{ level =>
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

object Layers {
  private val darkBlue = "#00002A"
  private val black = "#000000"
  private val green = "#00FF00"
  private val gameWidth = 32 * 16

  private val scoreTitle = new ScoreTitleLayer
  private val scoreLayer = new ScoreLayer
  private val scoreSpace = new BackgroundLayer(32 * 16, 16, darkBlue)
  private val homePlaceholder = new BackgroundLayer(32 * 16, 48, green)
  private val frog = new Sprite(Image("img/frog.png"), 22)

  private val statusLayer = new StatusLayer
  private val timeLayer = new TimeLayer

  private val top = Seq(scoreTitle, scoreLayer, scoreSpace, homePlaceholder)
  private val topPositions = VerticalCompositeLayout.layout(Vector(0, 0), top)
  private val topHeight = top.map(_.height).sum

  private val channels = Channel.channels(1)
  private val channelPositions = VerticalCompositeLayout.layout(Vector(0, topHeight), channels)
  private val channelsHeight = channels.map(_.height).sum

  private val bottom = Seq(statusLayer, timeLayer)
  private val bottomPositions = VerticalCompositeLayout.layout(Vector(0, topHeight + channelsHeight), bottom)

  private val homes = (0 to 5).map(HomeFactory.create(_))
  private val homesPositions = HorizontalCompositeLayout.layout(topPositions(3), homes)
  private val layers =
    new Layers(scoreTitle, scoreLayer, scoreSpace, homePlaceholder, frog, statusLayer, timeLayer, channels, homes)

  def allPositions(channelPositions: Seq[Vector]) =
    topPositions ++ channelPositions ++ bottomPositions ++ homesPositions

  def initialModel() = {
    val frogPos = channelPositions.last.add((gameWidth - frog.width) / 2, (32 - 22) / 2).add(0, -32*8)

    Model(positions = allPositions(channelPositions), frogPosition = frogPos, layers = layers)
  }
}
