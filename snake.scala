import scala.collection.immutable.{Vector => Vec}

class Snake{
    ???
}

class Apple() {
    ???
}

class Game(h: Int, w: Int) {

    val height = h
    val width = w

    def gameBoard(targetRows: Int, targetCols: Int): Vec[Vec[Char]] = {
        val empty2D = Vec.empty[Vec[Char]]
        val empty1D = Vec.empty[Char]
        val fillChar = ' '

        def helper(row: Int, col: Int, tempOut: Vec[Char], board: Vec[Vec[Char]]): Vec[Vec[Char]] = (row, col) match {
            case (row, col) if (row == targetRows && col == targetCols) => board 
            case (row, col) if (col == targetCols) => helper(row+1, 0, empty1D, board :+ tempOut)
            case (row, col) => helper(row, col+1, tempOut :+ fillChar, board)
        }

        helper(0, 0, empty1D, empty2D)
    }

    def render: Unit = {
        val board = gameBoard(height, width)
        def prettyPrint: Unit = println("+" + "-" * width + "+") 

        prettyPrint
        board.foreach(n => println("|" + n.mkString("") + "|" ))
        prettyPrint
    }

}

object SnakeGame extends App {
    val a = new Game(10,30)
    a.render
}