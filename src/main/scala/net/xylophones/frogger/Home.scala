package net.xylophones.frogger

object HomeFactory {

  // 240 px wide

  val cellDimensions = (4, 8)

  val deadlyMask =
    """
      |111111111111000000000000
      |111000000111111111111111
      |111000000111111111111111
    """.stripMargin

  val typeMask =
    """
      |HHHHHHHHHHHH------------
      |HHHHHHHHHHHHFFFFBBBBAAAA
      |HHHHHHHHHHHHFFFFBBBBAAAA
    """.stripMargin

  def createHome(id: Int) = {
    //typeMask.map
  }
}

class HomeCellType extends Enumeration {
  case class HomeCellTypeVal() extends super.Val(0)

  val Deadly, Safe = HomeCellTypeVal()
}

class HomeCell(deadly: Boolean, imageLocation: CellCoords) {

}

class Home(id: Int, tiles: Array[Array[HomeCell]]) extends TiledLayer(new TiledImage(new Image("img/top.png"), 4, 8), 3, 12, Array(tiles.map(_.id))) {