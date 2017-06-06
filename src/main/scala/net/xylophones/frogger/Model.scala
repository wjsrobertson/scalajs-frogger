package net.xylophones.frogger

object Config {
  val gameWidth = 512
  val gameHeight = 1200
  val frogWidth = 22
  val frogHeight = 22
  val channelHeight = 32
  val channelsHeight = channelHeight * 12
  val scorePanelHeight = 16 * 3
  val homeHeight = 16 * 3
  val bottomPanelHeight = 16 + 32
  val frogMinY = scorePanelHeight
  val frogMaxY = scorePanelHeight + homeHeight + channelsHeight - (channelHeight - frogHeight) / 2
  val frogDeathTime = 20
  val timeImageWidth = 64
  // TODO - replace all these 16's with a single val - smallTileHeight
  val timeHeight = 16
  val statusHeight = 16
  val scoreTitleHeight = 16
  val scoreHeight = 16
  val levelTimeLimitMs = 60000
  val pointsForJump = 10
  val darkBlue = "#00002A"
  val black = "#000000"
  val green = "#00FF00"
}

case class Model(score: Int = 0,
                 highScore: Int = 0,
                 lives: Int = 3,
                 level: Int = 1,
                 levelStartTimeMs: Long = System.currentTimeMillis(),
                 frogJumpTimer: Int = 0,
                 frogPosition: Vector = Vector(0, 0),
                 frogFacing: Direction.Dir = Direction.Up,
                 positions: Seq[Vector],
                 layers: Layers,
                 frogDeathTimer: Int = 0,
                 playState: PlayState.State = PlayState.InPlay,
                 homeContents: Seq[HomeContent.Content] = (0 to 5).map(_ => HomeContent.Empty)) {

  def inPlay() = lives > 0

  def inDeathAnimation() = frogDeathTimer > 0

  def levelDurationMs() = System.currentTimeMillis() - levelStartTimeMs

  def channelsWithPositions(): Seq[(Channel, Vector)] = {
    val channelsPos: Seq[(Vector)] = (layers.all zip positions)
      .filter(_._1.isInstanceOf[Channel])
      .map(_._2)

    layers.channels zip channelsPos
  }

  def homesWithPositions():  Seq[(Home, Vector)] =  layers.homes zip Layers.homesPositions
}

case class Layers(scoreTitle: ScoreTitleLayer,
                  scoreLayer: ScoreLayer,
                  scoreSpace: BackgroundLayer,
                  homePlaceholder: BackgroundLayer,
                  frog: Sprite,
                  statusLayer: StatusLayer,
                  timeLayer: TimeLayer,
                  channels: Seq[Channel],
                  homes: Seq[Home],
                  frogLayer: FrogLayer) {

  def all: Seq[Layer] =
    Seq(scoreTitle, scoreLayer, scoreSpace, homePlaceholder) ++
      channels ++
      Seq(statusLayer, timeLayer) ++
      homes
}

object PlayState {

  abstract sealed class State

  case object InPlay extends State

  case object FrogDeathAnimation extends State

  case object NotInPlay extends State

  val inGameStates = Seq(InPlay, FrogDeathAnimation)

  val all = Seq(InPlay, FrogDeathAnimation, NotInPlay)
}

object Direction {

  abstract sealed class Dir(val frame: Int, val vector: Vector)

  case object Up extends Dir(0, Vector(0, -1))

  case object Down extends Dir(1, Vector(0, 1))

  case object Left extends Dir(2, Vector(-1, 0))

  case object Right extends Dir(3, Vector(1, 0))

}

object HomeContent {

  abstract sealed class Content(val char: Character)

  case object Empty extends Content('W')

  case object Frog extends Content('F')

  case object Insect extends Content('B')

  case object Alligator extends Content('A')

}

// TODO - do away with this nasty object
object Layers {
  private val scoreTitle = new ScoreTitleLayer
  private val scoreLayer = new ScoreLayer
  private val scoreSpace = new BackgroundLayer(32 * 16, 16, Config.darkBlue)
  private val homePlaceholder = new BackgroundLayer(32 * 16, 48, Config.green)

  private val frog = new Sprite(Image("img/frog.png"), 22)
  private val deadFrog = new Sprite(Image("img/deadfrog.png"), 22)
  private val frogLayer = new FrogLayer(frog, deadFrog)

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

  private val homes = (0 to 5).map(HomeFactory.create)
  private val layers = new Layers(scoreTitle, scoreLayer, scoreSpace, homePlaceholder, frog, statusLayer, timeLayer,
    channels, homes, frogLayer)

  val homesPositions = HorizontalCompositeLayout.layout(topPositions(3), homes)

  val initialFrogPosition = channelPositions.last.add((Config.gameWidth - frog.width) / 2, (32 - 22) / 2)

  def allPositions(channelPositions: Seq[Vector]) =
    topPositions ++ channelPositions ++ bottomPositions ++ homesPositions

  def initialModel() = {
    Model(positions = allPositions(channelPositions), frogPosition = initialFrogPosition, layers = layers)
  }
}