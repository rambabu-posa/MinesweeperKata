/**
  * Created by andrew on 14/10/17.
  */

case class Minefield(minefield:String) {
  val width=minefield.indexOf("\n")+1
  val length=minefield.length
}

object MineSweeper {

  def revealMines(minefield:Minefield): String = {

    val width = minefield.width
    val length = minefield.length

    val north: (Int) => Int = (i: Int) => i - width
    val south: (Int) => Int = (i: Int) => i + width

    val east: (Int) => Int = (i: Int) => i + 1
    val west: (Int) => Int = (i: Int) => i - 1

    val northEast: (Int) => Int = (i: Int) => north(i) + 1
    val northWest: (Int) => Int = (i: Int) => north(i) - 1
    val southEast: (Int) => Int = (i: Int) => south(i) + 1
    val southWest: (Int) => Int = (i: Int) => south(i) - 1

    val surroundingCells = Seq(north, northEast, east, southEast, south, southWest, west, northWest)

    def hasMine(f: (Int) => Int, i: Int) = {
      try {
        if (minefield.minefield(f(i)) == '*') 1
        else 0
      } catch {
        case e: java.lang.StringIndexOutOfBoundsException => 0
        case e: Throwable => throw e
      }
    }

    (for (cell <- 0 until length) yield {
      val currentCellContent: String = minefield.minefield(cell).toString
      if (currentCellContent == "*" || currentCellContent == "\n") {
        currentCellContent
      }
      else {
        surroundingCells.map(hasMine(_, cell)).sum
      }
    }).mkString
  }

}

