package net.xylophones.frogger

class Model(score: Int = 0 ,
            lives: Int = 0 ,
            frogState: Int = 0,
            level: Int = 0,
            timer: Int = 0)

object GameModel {
  val darkBlue = "#00002A"
  val black = "#000000"
  val green = "#00FF00"

  val scoreTitle: Layer = new BackgroundLayer(32 * 16, 32, darkBlue)
  val scoreLayer = new BackgroundLayer(32 * 16, 32, darkBlue)
  val scoreSpace: Layer = new BackgroundLayer(32 * 16, 32, darkBlue)
  val homes = (0 to 5).map(HomeFactory.create(_))
  val homePlaceholder = new BackgroundLayer(32 * 16, 48, green)
  private val channels = Channel.channels(1)
  val lifeLayer: Layer = new BackgroundLayer(32 * 16, 16, black)
  val timeLayer = new BackgroundLayer(32 * 16, 32, black)

  // postition elements in correct order
  val vertical = Seq(scoreTitle, scoreLayer, scoreSpace, homePlaceholder) ++ channels ++ Seq( lifeLayer , timeLayer)

  val layers = vertical ++ homes
  val vPositions = VerticalCompositeLayout.layout(Vector(0, 0), vertical)
  val positions = vPositions ++ HorizontalCompositeLayout.layout(vPositions(3), homes)

}
