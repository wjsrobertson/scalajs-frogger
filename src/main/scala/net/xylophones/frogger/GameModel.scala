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
           level: Int = 0
           )

object GameModel {

  // TODO - use local coords and global coords for layers
  // this way mutability can be kept out of the layers
  // i.e. layers positions will not change but offset will be kept in
  // channels can have double width to make sure they never fall off the screen

  val scoreTitle: Layer = ???
  val scoreLayer = ScoreLayer
  val scoreSpace: Layer = ???
  val homes = (0 to 5).map(HomeFactory.create(_))
  val home = new HorizontalCompositeLayer(homes)
  val channels = new VerticalCompositeLayer(Seq())
  val lifeLayer: Layer = ???
  val timeLayer = TimeLayer

  // postition elements in correct order
  val order = Seq(scoreTitle, scoreLayer, scoreSpace, home, channels, lifeLayer, timeLayer)
  val yOffsets = Seq(scoreTitle, scoreLayer, scoreSpace, home, channels, lifeLayer, timeLayer)
    .map(_.height)
    .foldLeft(Seq[Int]()){ (acc: Seq[Int], add: Int) => (add + acc.sum) +: acc }
    .reverse
  order zip yOffsets foreach { case (l: Layer, y: Int) => l.moveTo(0, y) }

  def tick() = {

  }

}
