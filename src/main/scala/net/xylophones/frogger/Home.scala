package net.xylophones.frogger

// TODO - can this be replaced with a companion object?
object HomeFactory {

  val deadlyMask =
    """
      |1111111111111000000000000
      |1110000001111111111111111
      |1110000001111111111111111
    """.stripMargin

  val isDeadly = deadlyMask
    .filter(c => c == '0' || c == '1')
    .zipWithIndex
    .map(t => t._2 -> (t._1 == '1')).toMap

  val typeMask =
    """
      |HHHHHHHHHHHHH------------
      |HHHHWWWWHHHHHFFFFBBBBAAAA
      |HHHHWWWWHHHHHFFFFBBBBAAAA
    """.stripMargin.split("\n").map(_.trim).filter(_.length != 0)

  def getImageTiles(v: Char): Array[HomeCell] = {
    typeMask.zipWithIndex
      .map { case (line: String, row: Int) => (row, line.zipWithIndex) }
      .flatMap { case (row: Int, c: Seq[(Char, Int)]) => c map { x => (row, x._2, x._1) } }
      .filter(_._3 == v)
      .map { case (row, col, _) => new HomeCell(Tile(col, row)) }
  }

  private val free =
    """
      |HHHHHHHHHHHHH
      |HHHHXXXXHHHHH
      |HHHHXXXXHHHHH
    """.stripMargin.split("\n").map(_.trim).filter(_.length != 0)

  def getTiles(xChar: Char): Array[Array[HomeCell]] = {
    val homeTiles = getImageTiles('H')
    val midTiles = getImageTiles(xChar)

    var h = -1
    var x = -1

    free.map { line =>
      val col = line map { c =>
        if (c == 'H') {
          h += 1
          homeTiles(h)
        } else {
          x += 1
          midTiles(x)
        }
      }

      col.toArray
    }
  }

  def create(id: Int): Home = {
    new Home(id, getTiles('W'))
  }
}

class HomeCell(val tile: Tile)

class Home(id: Int, tiles: Array[Array[HomeCell]]) extends TiledLayer(new TiledImage(new Image("img/top3.png"), 8, 16), 3, 13, tiles.map { row => row.map { cell => cell.tile } }) {
  moveTo(id * columns * tileImage.tileWidth, y)
}