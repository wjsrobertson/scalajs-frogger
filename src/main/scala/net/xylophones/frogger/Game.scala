package net.xylophones.frogger

import org.scalajs.dom.raw.HTMLImageElement
import org.scalajs.dom.{CanvasRenderingContext2D, document}

class Image(src: String) {
  val element = document.createElement("img").asInstanceOf[HTMLImageElement]
  element.src = src
}

case class Vector(x: Int, y: Int)

trait Rectangular {
  def x: Int

  def y: Int

  def width: Int

  def height: Int

  def padding: Int = 0

  def centre: (Int, Int) = (x + width / 2, y + height / 2)

  def intersects(other: Rectangular) = {
    val minX = x + padding
    val maxX = x + width - padding
    val minY = y + padding
    val maxY = y + height - padding

    val otherMinX = other.x + other.padding
    val otherMaxX = other.x + other.width - other.padding
    val otherMinY = other.y + other.padding
    val otherMaxY = other.y + other.height - other.padding

    ((minY > otherMinY && minY < otherMaxY) || (maxY > otherMinY && maxY < otherMaxY)) &&
      ((minX > otherMinX && minX < otherMaxX) || (maxX > otherMinX && maxX < otherMaxX))
  }

  def contains(point: (Int, Int)) = {
    val px = point._1
    val py = point._2

    px >= x && py >= y && px < (x + width) && py < (y + height)
  }
}

class Rectangle(val x: Int,
                val y: Int,
                val width: Int,
                val height: Int,
                override val padding: Int = 0) extends Rectangular

abstract class Layer(val width: Int = 0,
                     val height: Int = 0)  {

  def draw(context: CanvasRenderingContext2D, offset: Vector)
}

trait CellCoords {
  def col: Int

  def row: Int
}

case class Tile(col: Int, row: Int, id: Int = 0)

class TiledImage(val image: Image, val tileWidth: Int, val tileHeight: Int)

class TiledLayer(protected val tileImage: TiledImage, protected val rows: Int,  val columns: Int, contents: Array[Array[Tile]]) extends Layer {
  protected val img = tileImage.image.element

  override val height = rows * tileImage.tileHeight

  override val width = columns * tileImage.tileWidth

  def getCell(row: Int, column: Int) = contents(row)(column)

  /*
  def rectangles(): Seq[Rectangle with CellCoords] = for {
    r <- 0 until rows
    c <- 0 until columns
    rX = x + (r * tileImage.tileWidth)
    rY = y + (c * tileImage.tileHeight)
  } yield new Rectangle(rX, rY, tileImage.tileWidth, tileImage.tileHeight) with CellCoords {
    val col = c
    val row = r
  }
  */

  override def draw(context: CanvasRenderingContext2D, position: Vector) = {
    for (row <- 0 until rows) {
      for (column <- 0 until columns) {
        val tile = contents(row)(column)

        val tileCol = tile.col
        val tileRow = tile.row

        val imgXOffset = tileCol * tileImage.tileWidth
        val imgYOffset = tileRow * tileImage.tileHeight

        val canvasOffsetX = position.x + (tileImage.tileWidth * column)
        val canvasOffsetY = position.y + (tileImage.tileHeight * row)

        context.drawImage(img, imgXOffset, imgYOffset, tileImage.tileWidth, tileImage.tileHeight, canvasOffsetX, canvasOffsetY, tileImage.tileWidth, tileImage.tileHeight)
      }
    }
  }
}

object HorizontalCompositeLayout  {
  def layout(position: Vector, children: Seq[Layer]): Seq[Vector] = {
    val localOffsets = children.map(_.width).foldLeft(Seq(0)){
      (acc: Seq[Int], h: Int) => (acc.head + h) +: acc }
      .reverse
      .dropRight(1)

    val offsets = localOffsets.map(_ + position.x)

    children zip offsets map {
      case (c, ox) => Vector(ox, position.y)
    }
  }
}

object VerticalCompositeLayout {
  def layout(position: Vector, children: Seq[Layer]): Seq[Vector] = {
    val localOffsets = children.map(_.height).foldLeft(Seq(0)) {
      (acc: Seq[Int], h: Int) => (acc.head + h) +: acc}
      .reverse
      .dropRight(1)

    val offsets = localOffsets.map(_ + position.y)

    children zip offsets map {
      case (c, oy) => Vector(position.x, oy)
    }
  }
}

class BackgroundLayer(val w: Int, val h: Int, colour: String) extends Layer(w, h) {
  override def draw(context: CanvasRenderingContext2D, position: Vector) = {
    context.fillStyle = colour
    context.strokeStyle = colour
    context.fillRect(position.x, position.y, width ,height)
  }
}

class Sprite(image: Image, frameWidth: Int) extends Layer {
  val padding = Math.max(1, frameWidth / 4)
  private val img = image.element
  private val numFrames: Int = image.element.width / frameWidth
  private var frame = 0

  override def draw(context: CanvasRenderingContext2D, position: Vector) = {
    val imgXOffset = frame * frameWidth
    context.drawImage(img, imgXOffset, 0, frameWidth, img.height, position.x, position.y, frameWidth, img.height)
  }

  def setFrame(frame: Int) = this.frame = frame % numFrames

  def apply(image: Image) = new Sprite(image, img.width)

  //def midPoint() = (x + frameWidth / 2, y + img.height / 2)
}
