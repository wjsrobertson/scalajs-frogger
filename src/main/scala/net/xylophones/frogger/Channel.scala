package net.xylophones.frogger

class Channel(contents: Array[Int], velocity: Int) extends TiledLayer(new Image("img/tiles.png"), 16, 16, contents) {

  private var currentOffset = 0
  private val scrollBoundary = -tileWidth

  def update = {
    moveBy(velocity, 0)

    val offsetPixels =
      if (velocity > 0 && (x + tileWidth) < scrollBoundary) scrollBoundary - tileWidth - x
      else if (velocity < 0 && x > scrollBoundary) scrollBoundary - x
      else 0

    val offsetCells = offsetPixels / tileWidth
    if (offsetCells != 0) {
      moveByCells(offsetCells)
      shuntCells(offsetCells)
    }
  }

  def isDeadlyCollision(sprite: Sprite) =
    spriteCell(sprite, CellType.Deadly, (s: Sprite,r: Rectangular) => r.intersects(sprite))

  def isLanding(sprite: Sprite) =
    spriteCell(sprite, CellType.Moving, (s: Sprite,r: Rectangular) => r.contains(sprite.midPoint()))

  private def spriteCell(sprite: Sprite, t: CellType.CellTypeVal, relationship: (Sprite, Rectangular) => Boolean)
    = getCellRectangles().zipWithIndex.map {
    case (r, i) if relationship(sprite, r) => contents(i)
  }.flatMap(Cell.fromIndex).map(_.cellType).contains(t)

  private def shuntCells(offset: Int): Unit = {
    currentOffset = (currentOffset + offset + contents.length) % contents.length
    for (i <- 0 to contents.length) {
      val index = (i + currentOffset) % contents.length
      setCell(i, contents(index))
    }
  }

  private def moveByCells(numCells: Int) = moveBy(numCells * tileWidth, 0)
}

object CellType extends Enumeration {
  case class CellTypeVal() extends super.Val(0)
  val Moving, Deadly, Safe = CellTypeVal()
}

object Cell extends Enumeration {
  case class CellVal(override val id: Int, cellType: CellType.CellTypeVal) extends super.Val(id)

  val Empty = CellVal(0, CellType.Safe)
  val Border = CellVal(1, CellType.Safe)
  val River = CellVal(2, CellType.Deadly)
  val Road = CellVal(3, CellType.Safe)
  val Car1 = CellVal(4, CellType.Deadly)
  val Car2 = CellVal(5, CellType.Deadly)
  val Car3 = CellVal(6, CellType.Deadly)
  val LogLeft = CellVal(7, CellType.Moving)
  val LogMiddle = CellVal(8, CellType.Moving)
  val LogRight = CellVal(9, CellType.Moving)
  val LorryLeft = CellVal(10, CellType.Deadly)
  val LorryRight = CellVal(11, CellType.Deadly)
  val Tractor = CellVal(12, CellType.Deadly)
  val Turtle = CellVal(13, CellType.Moving)
  val AlligatorTail = CellVal(14, CellType.Moving)
  val AlligatorMiddle = CellVal(15, CellType.Moving)
  val AlligatorHead = CellVal(16, CellType.Moving)
  val AlligatorOpen = CellVal(17, CellType.Deadly)

  def fromIndex(index: Int): Option[Cell.CellVal] = values.find(v => v.id == index).map(_.asInstanceOf[Cell.CellVal])
}