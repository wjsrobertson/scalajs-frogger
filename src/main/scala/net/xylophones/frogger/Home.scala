package net.xylophones.frogger

import net.xylophones.frogger.ChannelFactory.cell
import net.xylophones.frogger.HomeFactory.typeMask

import scala.collection.immutable
import scala.collection.mutable.Stack

object HomeFactory {

  val rows = 3
  val cols = 24

  val cellDimensions = (4, 8)

  val deadlyMask =
    """
      |111111111111000000000000
      |111000000111111111111111
      |111000000111111111111111
    """.stripMargin

  val isDeadly = deadlyMask
    .filter(c => c == '0' || c == '1')
    .zipWithIndex
    .map(t => t._2 -> (t._1 == '1')).toMap

  val typeMask =
    """
      |HHHHHHHHHHHH------------
      |HHHHWWWWHHHHFFFFBBBBAAAA
      |HHHHWWWWHHHHFFFFBBBBAAAA
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
      |HHHHHHHHHHHH
      |HHHHXXXXHHHH
      |HHHHXXXXHHHH
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

  def create(id: Int) = {
    new Home(id, getTiles('A'))
  }
}

class HomeCell(val tile: Tile)

class Home(id: Int, tiles: Array[Array[HomeCell]]) extends TiledLayer(new TiledImage(new Image("img/top.png"), 4, 8), 3, 12, tiles.map { row => row.map { cell => cell.tile } })