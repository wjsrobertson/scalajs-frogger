package net.xylophones.frogger
import org.scalajs.dom.CanvasRenderingContext2D

object ScoreLayer extends Layer {
  override def draw(context: CanvasRenderingContext2D): Unit = ???
}

object TimeLayer extends Layer {
  override def draw(context: CanvasRenderingContext2D): Unit = ???
}

class Model(score: Int = 0 ,
            lives: Int = 0 ,
            frogState: Int = 0,
            level: Int = 0,
            timer: Int = 0)
`
object GameModel {

  // TODO - use local coords and global coords for layers
  // this way mutability can be kept out of the layers
  // i.e. layers positions will not change but offset will be kept in
  // channels can have double width to make sure they never fall off the screen

  val darkBlue = "#00002A"
  val black = "#000000"

  val scoreTitle: Layer = new BackgroundLayer(0, 0, 32 * 16, 32, darkBlue)
  val scoreLayer = new BackgroundLayer(0, 0, 32 * 16, 32, darkBlue)
  val scoreSpace: Layer = new BackgroundLayer(0, 0, 32 * 16, 32, darkBlue)
  val homes = (0 to 5).map(HomeFactory.create(_))
  val home = new HorizontalCompositeLayer(homes)
  val channels = new VerticalCompositeLayer(ChannelFactory.channels(1))
  val lifeLayer: Layer = new BackgroundLayer(0, 0, 32 * 16, 16, black)
  val timeLayer = new BackgroundLayer(0, 0, 32 * 16, 32, black)

  // postition elements in correct order
  val order = Seq(scoreTitle, scoreLayer, scoreSpace, home, channels , lifeLayer , timeLayer)
  val heights = order.map(_.height)
  val yOffsets = heights
    .foldLeft(Seq[Int](0)){ (acc: Seq[Int], h: Int) => (h + acc.head) +: acc }
    .reverse

  (order zip yOffsets) foreach { case (l: Layer, y: Int) =>
    l.moveTo(0, y)
  }
}
