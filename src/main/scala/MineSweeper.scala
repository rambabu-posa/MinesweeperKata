/**
  * Created by andrew on 14/10/17.
  */

case class Minefield(minefield:String) {
  val width=minefield.indexOf("\n")+1
  val length=minefield.length
}

object MineSweeper {

  val north: (Int, Int) => Int = (i:Int, width:Int)=>i-width
  val south: (Int, Int) => Int = (i:Int, width:Int)=>i+width
  val east: (Int, Int) => Int = (i:Int, width:Int)=>i+1
  val west: (Int, Int) => Int = (i:Int, width:Int)=>i-1

  val northEast: (Int, Int) => Int = (i:Int, width:Int)=>north(i,width)+1
  val northWest: (Int, Int) => Int = (i:Int, width:Int)=>north(i,width)-1
  val southEast: (Int, Int) => Int = (i:Int, width:Int)=>south(i,width)+1
  val southWest: (Int, Int) => Int = (i:Int, width:Int)=>south(i,width)-1

  val centre=Seq(north,northEast,east,southEast,south,southWest,west,northWest)

  def revealMines(minefield:Minefield): String ={

    val width=minefield.width
    val length=minefield.length

    def hasMine(f:(Int,Int)=>Int, i:Int)={
      try {
        if (minefield.minefield(f(i, width)) == '*') 1
        else 0
      } catch {
        case e=>0
      }
    }

    val findMines: Seq[String] =for(cell<-0 to length-1) yield {
        val currentCell=minefield.minefield(cell).toString
        if (currentCell=="*" || currentCell== "\n"){
          currentCell
        }
        else {
          centre.map(hasMine(_,cell)).sum.toString
        }

    }
    findMines.mkString
  }
}

