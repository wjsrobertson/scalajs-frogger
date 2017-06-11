package net.xylophones.frogger

// TODO - can this be replaced with a companion object?
object HomeFactory {

  val deadlyMask =
    """
      |1111111111111000000000000
      |1110000001111111100001111
      |1110000001111111100001111
    """.stripMargin

  val isDeadly: Map[Int, Boolean] = deadlyMask
    .filter(c => c == '0' || c == '1')
    .zipWithIndex
    .map(t => t._2 -> (t._1 == '1')).toMap

  val typeMask =
    """
      |HHHHHHHHHHHHH------------
      |HHHHWWWWHHHHHFFFFIIIIAAAA
      |HHHHWWWWHHHHHFFFFIIIIAAAA
    """.stripMargin.split("\n").map(_.trim).filter(_.length != 0)

  // TODO - a bit unwieldy this is
  def getImageTiles(v: Char): Array[Tile] = {
    typeMask.zipWithIndex
      .map { case (line: String, row: Int) => (row, line.zipWithIndex) }
      .flatMap { case (row: Int, c: Seq[(Char, Int)]) => c map { x => (row, x._2, x._1) } }
      .filter(_._3 == v)
      .map { case (row, col, _) =>
        val cellDeadly = Set('F', 'A', 'H').contains(v)
        val cellType = if (cellDeadly) CellType.Deadly else CellType.Landable
        Tile(col, row, cellType)
      }
  }

  private val free =
    """
      |HHHHHHHHHHHHH
      |HHHHXXXXHHHHH
      |HHHHXXXXHHHHH
    """.stripMargin.split("\n").map(_.trim).filter(_.length != 0)

  def getTiles(xChar: Char): Array[Array[Tile]] = {
    val homeTiles = getImageTiles('H')
    val midTiles = getImageTiles(xChar)

    // TODO - get rid of these vars
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
    new Home(id, getTiles(HomeContent.Empty.char), HomeContent.Empty)
  }

  // TODO - replace with enum
  def create(id: Int, content: HomeContent.Content): Home = {
    new Home(id, getTiles(content.char), content)
  }
}

class Home(val id: Int, tiles: Array[Array[Tile]], val content: HomeContent.Content)
  extends TiledLayer(new TiledImage(Image("img/top.png"), 8, 16), 3, 13, tiles)
