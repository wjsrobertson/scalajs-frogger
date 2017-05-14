package net.xylophones.frogger

class Channel(contents: Array[Int], velocity: Int) extends TiledLayer(new Image("img/tiles.png"), 16, 16, contents) {

  private var currentOffset = 0

  val scrollBoundary = -tileWidth

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

  def isDeadlyCollision(sprite: Sprite) = getCellRectangles().zipWithIndex.map {
    case (v, i) if v.intersects(sprite) => i
  }.flatMap(Cell.fromIndex).map(_.cellType())


  private def shuntCells(offset: Int): Unit = {
    currentOffset = (currentOffset + offset + contents.length) % contents.length
    for (i <- 0 to contents.length) {
      val index = (i + currentOffset) % contents.length
      setCell(i, contents(index))
    }
  }

  private def moveByCells(numCells: Int) = moveBy(numCells * tileWidth, 0)

}

private object CellType extends Enumeration {
  type CellType = Value
  val Moving, Deadly, Safe = CellType
}

private object Cell extends Enumeration {
  case class Cell(override val id: Int, cellType: CellType.type) extends super.Val(id)

  val Empty = Cell(0, CellType.Safe)
  val Border = Cell(1, CellType.Safe)
  val River = Cell(2, CellType.Deadly)
  val Road = Cell(3, CellType.Safe)
  val Car1 = Cell(4, CellType.Deadly)
  val Car2 = Cell(5, CellType.Deadly)
  val Car3 = Cell(6, CellType.Deadly)
  val LogLeft = Cell(7, CellType.Moving)
  val LogMiddle = Cell(8, CellType.Moving)
  val LogRight = Cell(9, CellType.Moving)
  val LorryLeft = Cell(10, CellType.Deadly)
  val LorryRight = Cell(11, CellType.Deadly)
  val Tractor = Cell(12, CellType.Deadly)
  val Turtle = Cell(13, CellType.Moving)
  val AlligatorTail = Cell(14, CellType.Moving)
  val AlligatorMiddle = Cell(15, CellType.Moving)
  val AlligatorHead = Cell(16, CellType.Moving)
  val AlligatorOpen = Cell(17, CellType.Deadly)

  def fromIndex(index: Int): Option[Cell] = values.find(v => v.id == index).asInstanceOf[Cell]

}