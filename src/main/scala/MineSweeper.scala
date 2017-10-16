/**
  * Created by andrew on 14/10/17.
  */
object MineSweeper {
  def revealMines(minefield:String): String ={

    val width=minefield.indexOf("\n")
    val minefieldS=minefield.replaceAll("\n","")

    val length=minefieldS.length
    val north =(i:Int,width:Int)=>i-width
    val south =(i:Int,width:Int)=>i+width
    val east = (i:Int,width:Int)=>i+1
    val west = (i:Int,width:Int)=>i-1

    val northEast =(i:Int,width:Int)=>north(i,width)+1
    val northWest =(i:Int,width:Int)=>north(i,width)-1
    val southEast =(i:Int,width:Int)=>south(i,width)+1
    val southWest =(i:Int,width:Int)=>south(i,width)-1

    def hasMine(f:(Int,Int)=>Int)(implicit i:Int)={
      if(minefieldS(f(i,width))=='*') 1
      else 0
    }
    def numOfMines(cell:Int): Int ={
      implicit val i=cell-1
      cellPostion(cell) match {

          //single cell
        case "SingleCell" =>0

          //single row
        case "SingleRow" =>hasMine(east)+hasMine(west)
        case "SingleRowFirst"=>hasMine(east)
        case "SingleRowLast"=>hasMine(west)

          // single column

        case "SingleColumn" =>    hasMine(north)+hasMine(south)
        case "SingleColumnTop" => hasMine(south)
        case "SingleColumnBottom"=>hasMine(north)

          //standard grid

        case "TopLeft" =>
          hasMine(south) + hasMine(east) + hasMine(southEast) //topLeft
        case "TopRight"=>
          hasMine(south) + hasMine(west) + hasMine(southWest)  //topRight
        case "TopCentre"=>
          hasMine(south) + hasMine(southEast) + hasMine(southWest)  + hasMine(east) + hasMine(west)  // topCentre
        case "BottomRight"=>
          hasMine(north) + hasMine(northWest) +  hasMine(west) //bottomRight
        case "BottomLeft" =>
          hasMine(north) + hasMine(northEast) + hasMine(east) //bottomLeft
        case "BottomCentre"=>
          hasMine(north) + hasMine(northWest)+ hasMine(northEast) + hasMine(east) + hasMine(west) //bottomCentre
        case "LeftColumn"=>
          hasMine(north) + hasMine(south)+hasMine(northEast)+hasMine(southEast)+hasMine(east)
        case "RightColumn"=>
          hasMine(north) + hasMine(south) + hasMine(west) + hasMine(northWest) + hasMine(southWest)  //right
        case _ => hasMine(north) +  hasMine(south) + hasMine(east) + hasMine(west) +
          hasMine(northEast)+hasMine(northWest) + hasMine(southEast) + hasMine(southWest) //centre
      }
    }

    def cellPostion(square:Int)={
      (isTop(square),isBottom(square),isRight(square),isLeft(square)) match {

        case (true,true,false,false) => "SingleRow"
        case (true,true,false,true)  => "SingleRowFirst"
        case (true,true,true,false)  => "SingleRowLast"

        case (false,false,true,true) => "SingleColumn"
        case (true,false,true,true)  => "SingleColumnTop"
        case (false,true,true,true)  => "SingleColumnBottom"

        case (true,true,true,true)   => "SingleCell"
        case (true,false,false,true) => "TopLeft"
        case (true,false,true,false) => "TopRight"
        case (true,false,false,false)=> "TopCentre"
        case (false,true,true,false) => "BottomRight"
        case (false,true,false,true) => "BottomLeft"
        case (false,true,false,false)=> "BottomCentre"
        case (false,false,false,true)=> "LeftColumn"
        case (false,false,true,false)=> "RightColumn"
        case _=> "Centre"
      }
    }

    def isTop(a:Int)={
      a<=width
    }
    def isLeft(a:Int)={
      (a-1) % width==0
    }
    def isRight(a:Int)={
      a % width==0
    }
    def isBottom(a:Int)={
      (a-1)>=length-width
    }

    val result: Seq[String] =for(x<-1 to length) yield {
      val cell=
        if (minefieldS(x-1)=='*') "*"
        else numOfMines(x).toString
      if ((x%width)==0) cell +"\n"
      else cell
    }
    result.mkString
  }
}
