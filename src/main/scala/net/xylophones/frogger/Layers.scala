package net.xylophones.frogger

object Direction {

  abstract sealed class Dir(val frame: Int, val vector: Vector)

  case object Up extends Dir(0, Vector(0, -1))

  case object Down extends Dir(1, Vector(0, 1))

  case object Left extends Dir(2, Vector(-1, 0))

  case object Right extends Dir(3, Vector(1, 0))

}

case class Model(score: Int = 0,
                 lives: Int = 0,
                 frogState: Int = 0,
                 level: Int = 0,
                 timer: Int = 0,
                 frogJumpTimer: Int = 0,
                 frogPosition: Vector = Vector(0, 0),
                 frogFacing: Direction.Dir = Direction.Up,
                 positions: Seq[Vector],
                 layers: Layers)

case class Layers(scoreTitle: BackgroundLayer,
                  scoreLayer: BackgroundLayer,
                  scoreSpace: BackgroundLayer,
                  homePlaceholder: BackgroundLayer,
                  frog: Sprite,
                  lifeLayer: BackgroundLayer,
                  timeLayer: BackgroundLayer,
                  channels: Seq[Channel],
                  homes: Seq[Home]) {

  val all: Seq[Layer] =
    Seq(scoreTitle, scoreLayer, scoreSpace, homePlaceholder) ++
      channels ++
      Seq(lifeLayer, timeLayer) ++
      homes
}

object Layers {
  private val darkBlue = "#00002A"
  private val black = "#000000"
  private val green = "#00FF00"
  private val gameWidth = 32 * 16

  private val scoreTitle = new BackgroundLayer(32 * 16, 32, darkBlue)
  private val scoreLayer = new BackgroundLayer(32 * 16, 32, darkBlue)
  private val scoreSpace = new BackgroundLayer(32 * 16, 32, darkBlue)
  private val homePlaceholder = new BackgroundLayer(32 * 16, 48, green)
  private val frog = new Sprite(new Image("img/frog.png"), 22)

  private val lifeLayer = new BackgroundLayer(32 * 16, 16, black)
  private val timeLayer = new BackgroundLayer(32 * 16, 32, black)

  private val top = Seq(scoreTitle, scoreLayer, scoreSpace, homePlaceholder)
  private val topPositions = VerticalCompositeLayout.layout(Vector(0, 0), top)
  private val topHeight = top.map(_.height).sum

  private val channels = Channel.channels(1)
  private val channelPositions = VerticalCompositeLayout.layout(Vector(0, topHeight), channels)
  private val channelsHeight = channels.map(_.height).sum

  private val bottom = Seq(lifeLayer, timeLayer)
  private val bottomPositions = VerticalCompositeLayout.layout(Vector(0, topHeight + channelsHeight), bottom)

  private val homes = (0 to 5).map(HomeFactory.create(_))
  private val homesPositions = HorizontalCompositeLayout.layout(topPositions(3), homes)
  private val layers =
    new Layers(scoreTitle, scoreLayer, scoreSpace, homePlaceholder, frog, lifeLayer, timeLayer, channels, homes)

  def allPositions(channelPositions: Seq[Vector]) =
    topPositions ++ channelPositions ++ bottomPositions ++ homesPositions

  def initialModel() = {
    val frogPos = channelPositions.last.add((gameWidth - frog.width) / 2, (32 - 22) / 2)

    Model(positions = allPositions(channelPositions), frogPosition = frogPos, layers = layers)
  }
}
