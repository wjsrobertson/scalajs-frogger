package net.xylophones.frogger

case class Model(score: Int = 0,
            lives: Int = 0,
            frogState: Int = 0,
            level: Int = 0,
            timer: Int = 0,
            positions: Seq[Vector])

object GameModel {
  val darkBlue = "#00002A"
  val black = "#000000"
  val green = "#00FF00"

  val scoreTitle: Layer = new BackgroundLayer(32 * 16, 32, darkBlue)
  val scoreLayer = new BackgroundLayer(32 * 16, 32, darkBlue)
  val scoreSpace: Layer = new BackgroundLayer(32 * 16, 32, darkBlue)
  val homePlaceholder = new BackgroundLayer(32 * 16, 48, green)

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

  println(s"ch: ${channels(0).columns}")

  def initialModel() = {
    (layers, positions, new Model(positions = allPositions(channelPositions)))
  }
}
