/**
  * Created by andrew on 17/10/17.
  */
object Game extends App {
  val mines = Minefield("*---*\n" +
    "-----\n" +
    "-----\n" +
    "-----\n" +
    "*---*\n")

  println(MineSweeper.revealMines(mines))
}
