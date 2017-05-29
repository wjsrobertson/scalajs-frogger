package net.xylophones.frogger
import org.scalajs.dom.CanvasRenderingContext2D

class Model(score: Int = 0 ,
            lives: Int = 0 ,
            frogState: Int = 0,
            level: Int = 0,
            timer: Int = 0)

object GameModel {

  // TODO - use local coords and global coords for layers
  // this way mutability can be kept out of the layers
  // i.e. layers positions will not change but offset will be kept in
  // channels can have double width to make sure they never fall off the screen

  val darkBlue = "#00002A"
  val black = "#000000"
  val green = "#00FF00"

  val scoreTitle: Layer = new BackgroundLayer(32 * 16, 32, darkBlue)
  val scoreLayer = new BackgroundLayer(32 * 16, 32, darkBlue)
  val scoreSpace: Layer = new BackgroundLayer(32 * 16, 32, darkBlue)
  val homes = (0 to 5).map(HomeFactory.create(_))
  val homePlaceholder = new BackgroundLayer(32 * 16, 48, green)
  private val channels = ChannelFactory.channels(1)
  val lifeLayer: Layer = new BackgroundLayer(32 * 16, 16, black)
  val timeLayer = new BackgroundLayer(32 * 16, 32, black)

  // postition elements in correct order
  val vertical = Seq(scoreTitle, scoreLayer, scoreSpace, homePlaceholder) ++ channels ++ Seq( lifeLayer , timeLayer)

  val layers = vertical ++ homes
  val vPositions = VerticalCompositeLayout.layout(Vector(0, 0), vertical)
  val positions = vPositions ++ HorizontalCompositeLayout.layout(vPositions(3), homes)

}
