package net.xylophones.frogger

object FrogFacing {
  class Direction(val frame: Int, val vector: Vector)

  case object Up extends Direction(0, Vector(0, -1))

  case object Down extends Direction(1, Vector(0, 1))

  case object Left extends Direction(2, Vector(-1, 0))

  case object Right extends Direction(3, Vector(1, 0))
}

case class Model(score: Int = 0,
                 lives: Int = 0,
                 frogState: Int = 0,
                 level: Int = 0,
                 timer: Int = 0,
                 frogJumpTimer: Int = 0,
                 frogPosition: Vector = Vector(0,0),
                 frogFacing: FrogFacing.Direction = FrogFacing.Up,
                 positions: Seq[Vector])

object GameModel {
  val darkBlue = "#00002A"
  val black = "#000000"
  val green = "#00FF00"
  val gameWidth = 32 * 16

  val scoreTitle = new BackgroundLayer(32 * 16, 32, darkBlue)
  val scoreLayer = new BackgroundLayer(32 * 16, 32, darkBlue)
  val scoreSpace: Layer = new BackgroundLayer(32 * 16, 32, darkBlue)
  val homePlaceholder = new BackgroundLayer(32 * 16, 48, green)
  val frog = new Sprite(new Image("img/frog.png"), 22)

  val lifeLayer: Layer = new BackgroundLayer(32 * 16, 16, black)
  val timeLayer = new BackgroundLayer(32 * 16, 32, black)

  private val top = Seq(scoreTitle, scoreLayer, scoreSpace, homePlaceholder)
  private val topPositions = VerticalCompositeLayout.layout(Vector(0, 0), top)
  private val topHeight = top.map(_.height).sum

  val channels = Channel.channels(1)
  val channelPositions = VerticalCompositeLayout.layout(Vector(0, topHeight), channels)
  private val channelsHeight = channels.map(_.height).sum

  private val bottom = Seq(lifeLayer, timeLayer)
  val bottomPositions = VerticalCompositeLayout.layout(Vector(0, topHeight + channelsHeight), bottom)

  private val homes = (0 to 5).map(HomeFactory.create(_))
  private val homesPositions = HorizontalCompositeLayout.layout(topPositions(3), homes)

  val layers = top ++ channels ++ bottom ++ homes
  val positions = topPositions ++ channelPositions ++ bottomPositions ++ homesPositions

  def allPositions(channelPositions: Seq[Vector]) = topPositions ++ channelPositions ++ bottomPositions ++ homesPositions

  def initialModel() = {
    val frogPos = channelPositions.last.add( (gameWidth-frog.width)/2, (32 - 22)/2 )
    (layers, positions, new Model(positions = allPositions(channelPositions), frogPosition = frogPos))
  }
}
