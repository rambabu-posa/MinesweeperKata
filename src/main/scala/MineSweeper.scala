/**
  * Created by andrew on 14/10/17.
  */


object MineSweeper {

  val north: (Int, Int) => Int = (i:Int, width:Int)=>i-width
  val south: (Int, Int) => Int = (i:Int, width:Int)=>i+width
  val east: (Int, Int) => Int = (i:Int, width:Int)=>i+1
  val west: (Int, Int) => Int = (i:Int, width:Int)=>i-1

  val northEast: (Int, Int) => Int = (i:Int, width:Int)=>north(i,width)+1
  val northWest: (Int, Int) => Int = (i:Int, width:Int)=>north(i,width)-1
  val southEast: (Int, Int) => Int = (i:Int, width:Int)=>south(i,width)+1
  val southWest: (Int, Int) => Int = (i:Int, width:Int)=>south(i,width)-1

  val self: (Int, Int) => Int = (i:Int, width:Int)=>i

  val singleCell=Seq(self)

  val singleRow=Seq(east,west)
  val singleRowFirst=Seq(east)
  val singleRowLast=Seq(west)

  val singleColumn=Seq(north,south)
  val singleColumnTop=Seq(south)
  val singleColumnBottom=Seq(north)

  val topLeft=Seq(south,east,southEast)
  val topRight=Seq(south,west,southWest)
  val topCentre=Seq(west,southWest,south,southEast,east)

  val bottomLeft=Seq(north,northEast,east)
  val bottomRight=Seq(north,northWest,west)
  val bottomCentre=Seq(west,northWest,north,northEast,east)

  val leftColumn=Seq(north,northEast,east,southEast,south)
  val rightColumn=Seq(north,northWest,west,southWest,south)
  val centre=Seq(north,northEast,east,southEast,south,southWest,west,northWest)

  def revealMines(minefield:String): String ={

    val width=minefield.indexOf("\n")+1
    val length=minefield.length

    def isTop(implicit currentCell:Int)={
      currentCell<=width
    }
    def isLeft(implicit currentCell:Int)={
      (currentCell-1) % width==0
    }
    def isRight(implicit currentCell:Int)={
      currentCell % width==0
    }
    def isBottom(implicit currentCell:Int)={
      currentCell-1>=length-width
    }

    def cellsToCheck(implicit square:Int): Seq[(Int, Int) => Int] ={

      (isTop,isBottom,isRight,isLeft) match {

        case (true,true,true,true)   => singleCell

        case (true,true,false,false) => singleRow
        case (true,true,false,true)  => singleRowFirst
        case (true,true,true,false)  => singleRowLast

        case (false,false,true,true) => singleColumn
        case (true,false,true,true)  => singleColumnTop
        case (false,true,true,true)  => singleColumnBottom

        case (true,false,false,true) => topLeft
        case (true,false,true,false) => topRight
        case (true,false,false,false)=> topCentre
        case (false,true,true,false) => bottomRight
        case (false,true,false,true) => bottomLeft
        case (false,true,false,false)=> bottomCentre
        case (false,false,false,true)=> leftColumn
        case (false,false,true,false)=> rightColumn

        case _=> centre
      }
    }

    def hasMine(f:(Int,Int)=>Int, i:Int)={
      if(minefield(f(i,width))=='*') 1
      else 0
    }

    val findMines: Seq[String] =for(cell<-0 to length-1) yield {
        val currentCell=minefield(cell).toString
        if (currentCell=="*" || currentCell== "\n"){
          currentCell
        }
        else {
          cellsToCheck(cell+1).map(hasMine(_,cell)).sum.toString
        }

    }

    findMines.mkString
  }
}

