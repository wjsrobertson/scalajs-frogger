package net.xylophones.frogger

class Channel(val tiles: Array[Cell.CellVal], val velocity: Int)
  extends TiledLayer(
    new TiledImage(new Image("img/tiles.png"), 32, 32), 1, tiles.length, Array(tiles.map(t => Tile(t.id, 0)))) {
}

object Channel {
  def channels(level: Int): Seq[Channel] = {
    levels(level).split("\n")
      .map(_.trim)
      .filterNot(_.length == 0)
      .map { c => c.split("\t") }
      .map ( x => (x(0).toInt, x(1)) )
      .map { case (velocity, contents) => (velocity.toInt, (contents * 2).split("").map(cell)) }
      .map { case (velocity, cells) => new Channel(cells, velocity) }
  }

  private val cell = Map(
    "%" -> Cell.Border,
    "~" -> Cell.River,
    "(" -> Cell.LogLeft,
    ")" -> Cell.LogRight,
    "=" -> Cell.LogMiddle,
    "@" -> Cell.Turtle,
    "." -> Cell.Road,
    "[" -> Cell.LorryLeft,
    "]" -> Cell.LorryRight,
    "{" -> Cell.Car1,
    ">" -> Cell.Car3,
    "<" -> Cell.Car2,
    "}" -> Cell.Tractor,
    "-" -> Cell.AlligatorTail,
    "+" -> Cell.AlligatorMiddle,
    "*" -> Cell.AlligatorHead,
    "#" -> Cell.AlligatorOpen
  )

  private val levels = Seq(
    """
      |1	~~~~~(==)~~(==)~~(==)~~
      |-3	~@@@~@@@~@@@~@@@~~~~~~~~~~~~
      |2	(===)~~(===)~~(===)~~
      |3	~~~~~~(=)~~(=)~~(=)~~
      |-3	~@@@~@@@~@@@~@@@~@@@~@@@~~~~~~
      |0	%%%%%%%%%%%%%%%%
      |-1	...[]...[]...[]...
      |1	...............>
      |-2	...{...{...{......
      |2	..........}...}...}
      |-1	<...<...<.........
      |0	%%%%%%%%%%%%%%%%
    """.stripMargin,
    """
      |1	~~~~~-+*~~~(=)~~~(==)~~
      |-3	~@@~~@@@~@@@~@@~~~~~~~~~~~~~
      |3	(==)~~~(===)~~~~(==)~~
      |3	~~~~~~(=)~~(=)~~(=)~~~~
      |-3	~~@@~@@~~@@@~@@@~~@@~@@@~~~~~~
      |0	%%%%%%%%%%%%%%%%
      |-1	...[]...[]...[]...
      |7	...>.>..........
      |-2	...{...{...{......
      |1	...}......}...}...}
      |-1	<...<...<...<.....
      |0	%%%%%%%%%%%%%%%%
    """.stripMargin,
    """
      |2	~~~~~-+#~~~(=)~~~(==)~~
      |-4	~@@~~@@@~@@~~@@~~~~~~~~~~~~~
      |3	(=)~~~~(===)~~~~(==)~~
      |4	~~~~~~()~~~(=)~~(=)~~~~
      |-4	~~@@~@@~~~@@~@@@~~@@~@@@~~~~~~
      |0	%%%%%%%%%%%%%%%%
      |-1	...[]...[]...[]...
      |2	..>>.>.........>
      |-3	...{...{...{.....{
      |1	...}..}...}...}...}
      |-2	<...<...<...<...<.
      |0	%%%%%%%%%%%%%%%%
    """.stripMargin,
    """
      |3	~~~~~-+#~~~(=)~~~(==)~~
      |-4	@~~~~@@~~@@~~@@~~~~~~@
      |3	-+*~~~~(===)~~~~(==)~~
      |5	~~~~~~()~~~()~~~(=)~~~~
      |-4	~~~@~@@~~~@@~@@@~~@@~~@~~~
      |0	%%%%%%%%%%%%%%%%
      |-3	...[]...[]...[].[]
      |7	...>.>.......>..
      |-4	...{...{...{.....{
      |4	}..}.}..........
      |-2	<..<..<.<...<...<.
      |0	%%%%%%%%%%%%%%%%
    """.stripMargin,
    """
      |4	~~~~~-+#~~~-+#~~~(=)~~
      |-5	@~~~~~~@@~~@@~~@@~~~~~~@
      |4	-+#~~~~(=)~~~~-+#~~
      |7	~~~~~~()~~~()~~~-+#~~~~
      |-5	~~~@~@@~~~@@~@@~~~@@~~@~~~~~
      |0	%%%%%%%%%%%%%%%%
      |-4	..[]..[]...[].[]..[]
      |3	...>.>....>..>..
      |-5	...{...{.{.{.....{
      |3	}..}.}......}...
      |-3	<.<..<.<...<..<.<..<.
      |0	%%%%%%%%%%%%%%%%
    """.stripMargin)
}

object ChannelCollisionChecker {

  def isDeadlyCollision(channel: Channel, chPosition: Vector, sprite: Sprite, sPosition: Vector) = {
    channel.rectangles
      .filter(r => r.intersects(chPosition, sprite, sPosition))
      .map(r => channel.tiles(r.col))
      .map(_.cellType)
      .contains(CellType.Deadly)
  }
  //  spriteCell(channel, sprite, CellType.Deadly, (s: Sprite, r: Rectangular) => r.intersects(chPosition, sprite, sPosition))

  def isLanding(channel: Channel, chPosition: Vector, sprite: Sprite, sPosition: Vector) = {
    channel.rectangles
      .filter(r => r.contains(chPosition, sprite.midPoint, sPosition))
      .map(r => channel.tiles(r.col))
      .map(_.cellType)
      .contains(CellType.Moving)
  }
  //    spriteCell(channel, sprite, CellType.Moving, (s: Sprite, r: Rectangular) => r.contains(chPosition, sprite.midPoint, sPosition))
/*
  private def spriteCell(channel: Channel, chPos:Vector, sprite: Sprite, sPos: Vector, t: CellType.CellTypeVal, relationship: (Sprite, Vector, Rectangular, Vector) => Boolean) =
    channel.rectangles
      .filter(r => relationship(sprite, r))
      .map(r => channel.tiles(r.col))
      .map(_.cellType)
      .contains(t)
  */
}

object CellType extends Enumeration {
  case class CellTypeVal() extends super.Val()

  val Moving, Deadly, Safe = CellTypeVal()
}

object Cell extends Enumeration {

  case class CellVal(override val id: Int, cellType: CellType.CellTypeVal) extends super.Val(id)

  //val Empty = CellVal(0, CellType.Safe)
  val Border = CellVal(0, CellType.Safe)
  val River = CellVal(1, CellType.Deadly)
  val Road = CellVal(2, CellType.Safe)
  val Car1 = CellVal(3, CellType.Deadly)
  val Car2 = CellVal(4, CellType.Deadly)
  val Car3 = CellVal(5, CellType.Deadly)
  val LogLeft = CellVal(6, CellType.Moving)
  val LogMiddle = CellVal(7, CellType.Moving)
  val LogRight = CellVal(8, CellType.Moving)
  val LorryLeft = CellVal(9, CellType.Deadly)
  val LorryRight = CellVal(10, CellType.Deadly)
  val Tractor = CellVal(11, CellType.Deadly)
  val Turtle = CellVal(12, CellType.Moving)
  val AlligatorTail = CellVal(13, CellType.Moving)
  val AlligatorMiddle = CellVal(14, CellType.Moving)
  val AlligatorHead = CellVal(15, CellType.Moving)
  val AlligatorOpen = CellVal(16, CellType.Deadly)

  def fromIndex(index: Int): Option[Cell.CellVal] = values.find(v => v.id == index).map(_.asInstanceOf[Cell.CellVal])
}