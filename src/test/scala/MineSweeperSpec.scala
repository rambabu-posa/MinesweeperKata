import org.scalatest.{FlatSpec, Matchers, WordSpecLike, _}
import org.scalatest.prop.TableDrivenPropertyChecks._

class MineSweeperSpec extends WordSpecLike with Matchers {

  val singleCell=Table(
    ("Minefield","RevealledMinefield"),
    ("-\n",           "0"),
    ("*\n",           "*")
  )

  val singleRow=Table(
    ("Minefield","RevealledMinefield"),
    ("--\n",           "00"),
    ("**\n",            "**"),
    ("*-\n",           "*1"),
    ("-*\n",           "1*"),
    ("*-------*\n",    "*1000001*"),
    ("*---*---*\n",    "*101*101*")

  )

  val singleColumn=Table(
    ("Minefield","RevealledMinefield"),
    ("-\n-\n",           "00"),
    ("*\n*\n",            "**"),
    ("*\n-\n",           "*1"),
    ("-\n*\n",           "1*"),
    ("*\n-\n-\n-\n-\n-\n-\n-\n*\n",    "*1000001*"),
    ("*\n-\n-\n-\n*\n-\n-\n-\n*\n",    "*101*101*")
   )
  val regularMinefield=Table(
    ("Minefield","RevealledMinefield"),
    ("----\n----\n----\n----\n----\n","00000000000000000000"),
    ("****\n****\n****\n****\n****\n","********************"),
    ("*----\n*----\n*----\n*----\n*----\n","*2000*3000*3000*3000*2000"),
    ("----*\n----*\n----*\n----*\n----*\n","0002*0003*0003*0003*0002*"),
    ("*----\n-----\n-----\n-----\n-----\n","*100011000000000000000000"),
    ("*---*\n-----\n-----\n-----\n*---*\n","*101*110110000011011*101*"),
    ("*---*\n-----\n--*--\n-----\n*---*\n","*101*1212101*1012121*101*"),
    ("*****\n*---*\n*---*\n*---*\n*****\n","******535**303**535******"),
    ("-----\n-*-*-\n*-*-*\n-*-*-\n-----\n","112112*3*2*4*4*2*3*211211"),
//    ("*----\n*----\n*----\n*----\n*----\n","*2000*3000*3000*3000*2000"),
//    ("*----\n-----\n-----\n-----\n-----\n","*100011000000000000000000"),
    ("-----\n**---\n-----\n--*--\n-----\n","22100**1002321001*1001110")
  )


  "Calling Minesweeper" should {
    forAll(singleCell) { (minefield: String, revealledMineField) =>
      s"return $revealledMineField where minefield is $minefield" in {
        val result = MineSweeper.revealMines(minefield)
        result shouldBe revealledMineField
      }
    }

    forAll(singleRow) { (minefield: String, revealledMineField) =>
      s"return $revealledMineField where minefield is $minefield" in {
        val result = MineSweeper.revealMines(minefield)
        result shouldBe revealledMineField
      }
    }

    forAll(singleColumn) { (minefield: String, revealledMineField) =>
      s"return $revealledMineField where minefield is $minefield" in {
        val result = MineSweeper.revealMines(minefield)
        result shouldBe revealledMineField
      }
    }

    forAll(regularMinefield){(minefield:String,revealledMineField)=>
       s"return $revealledMineField where minefield is $minefield" in {
          val result=MineSweeper.revealMines(minefield)
          result shouldBe revealledMineField
       }
    }
  }
}
