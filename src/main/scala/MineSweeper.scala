import scala.util.{Failure, Success, Try}

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

    val north: (Int) => Int = (cellPosition: Int) => cellPosition - width
    val south: (Int) => Int = (cellPosition: Int) => cellPosition + width

    val east: (Int) => Int = (cellPosition: Int) => cellPosition + 1
    val west: (Int) => Int = (cellPosition: Int) => cellPosition - 1

    val northEast: (Int) => Int = (cellPosition: Int) => north(cellPosition) + 1
    val northWest: (Int) => Int = (cellPosition: Int) => north(cellPosition) - 1
    val southEast: (Int) => Int = (cellPosition: Int) => south(cellPosition) + 1
    val southWest: (Int) => Int = (cellPosition: Int) => south(cellPosition) - 1

    val surroundingCells = Seq(north, northEast, east, southEast, south, southWest, west, northWest)

    def hasMine(f: (Int) => Int, cell: Int):Try[Int] = {
      Try {
        if (minefield.minefield(f(cell)) == '*') 1
        else 0
      }
    }

    (for (cell <- 0 until length) yield {
      val currentCellContent: String = minefield.minefield(cell).toString
      if (currentCellContent == "*" || currentCellContent == "\n") {
        currentCellContent
      }
      else {
        surroundingCells.map(hasMine(_, cell) match {
          case Success(i) => i
          case Failure(e) => 0
        }).sum
      }
    }).mkString
  }
}

