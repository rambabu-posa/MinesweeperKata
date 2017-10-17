/**
  * Created by andrew on 14/10/17.
  */

object minefieldDefinitions {

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

}

object MineSweeper {
  import minefieldDefinitions._

  def revealMines(minefield:String): String ={

    val width=minefield.indexOf("\n")
    val minefieldS=minefield.replaceAll("\n","")

    val length=minefieldS.length

    def hasMine(f:(Int,Int)=>Int)(implicit i:Int)={
      if(minefieldS(f(i,width))=='*') 1
      else 0
    }

    def cellsToCheck(square:Int): Seq[(Int, Int) => Int] ={
      implicit val x=square
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

    val findMines: Seq[String] =for(cell<-1 to length) yield {
      implicit val i=cell-1
      val c=
        if (minefieldS(cell-1)=='*') "*"
        else cellsToCheck(cell).map(hasMine(_)).sum.toString
      if ((cell%width)==0) c +"\n"
      else c
    }
    findMines.mkString
  }
}

