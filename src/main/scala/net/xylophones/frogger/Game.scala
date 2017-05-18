package net.xylophones.frogger

import org.scalajs.dom.raw.HTMLImageElement
import org.scalajs.dom.{CanvasRenderingContext2D, document}

class Image(src: String) {
  val element = document.createElement("img").asInstanceOf[HTMLImageElement]
  element.src = src
}

trait Rectangular {
  def x: Int

  def y: Int

  def width: Int

  def height: Int

  def padding: Int

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
                val padding: Int = 0) extends Rectangular

abstract class Layer(private var xPos: Int = 0,
                     private var yPos: Int = 0,
                     val width: Int = 0,
                     val height: Int = 0) extends Rectangular {

  def x = xPos

  def y = yPos

  def moveTo(x: Int, y: Int) = {
    this.xPos = x
    this.yPos = y
  }

  def moveBy(dx: Int, dy: Int) = {
    xPos += dx
    yPos += dy
  }

  def draw(context: CanvasRenderingContext2D)
}

class TiledLayer(image: Image, val tileWidth: Int, /*rows: Int,*/ columns: Int, contents: Array[Int]) extends Layer {
  val padding = 0
  private val img = image.element

  def setCell(column: Int, id: Int) = contents(column) = id

  def getCell(column: Int) = contents(column)

  def getCellRectangles(): Seq[Rectangular] = contents.indices.map {
    i => new Rectangle(x+(i*tileWidth), y, tileWidth, img.height)
  }

  override def draw(context: CanvasRenderingContext2D) = {
    for (column <- 0 until columns) {
      val index = contents(column)
      val imgXOffset = index * tileWidth
      val canvasOffsetX = x + (tileWidth * column)
      context.drawImage(img, imgXOffset, 0, tileWidth, img.height, canvasOffsetX, y, tileWidth, img.height)
    }
  }
}

class Sprite(image: Image, frameWidth: Int) extends Layer {
  val padding = Math.max(1, frameWidth / 4)
  private val img = image.element
  private val numFrames: Int = image.element.width / frameWidth
  private var frame = 0

  override def draw(context: CanvasRenderingContext2D) = {
    val imgXOffset = frame * frameWidth
    context.drawImage(img, imgXOffset, 0, frameWidth, img.height, x, y, frameWidth, img.height)
  }

  def setFrame(frame: Int) = this.frame = frame % numFrames

  def apply(image: Image) = new Sprite(image, img.width)

  def midPoint() = (x + frameWidth/2, y + img.height/2)
}
