import scala.collection.immutable.{Vector => Vec}

class Snake(headCord: (Int, Int), bodyCords: Seq[(Int, Int)], direction: String) {
    def body: Seq[(Int, Int)] = bodyCords
    def head: (Int, Int) = headCord
    def dir: String = direction

    def next(input: Char): Snake = {
        val newDir = input match {
            case 'a' if ("right" != direction) => "left"
            case 'w' if ("down" != direction) => "up"
            case 'd' if ("left" != direction) => "right"
            case 's' if ("up" != direction) => "down"
            case _ => direction
        }

        newDir match {
            case "left" => new Snake( (headCord._1-1, headCord._2), headCord +: bodyCords.init, newDir )
            case "up" => new Snake( (headCord._1, headCord._2-1), headCord +: bodyCords.init, newDir )
            case "right" => new Snake( (headCord._1+1, headCord._2), headCord +: bodyCords.init, newDir )
            case "down" => new Snake( (headCord._1, headCord._2+1), headCord +: bodyCords.init, newDir )
        }
    }
}

class Apple() {
    ???
}

class Game(h: Int, w: Int) {

    val height = h
    val width = w

    val initSnakeBody = Seq((14,5), (15,5), (16,5))
    val initSnake = new Snake( (13,5), initSnakeBody, "left")
    val emptyBoard = initialBoard(height, width)

    private def initialBoard(targetRows: Int, targetCols: Int): Vec[Vec[Char]] = {
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

    private def snakeBoard(snake: Snake, board: Vec[Vec[Char]]): Vec[Vec[Char]] = {
        val bodylessBoard = board.updated(snake.head._2,
                            board(snake.head._2).updated(snake.head._1, 'X'))

        def helper(snakePosSeq: Seq[(Int, Int)], output: Vec[Vec[Char]]): Vec[Vec[Char]] = snakePosSeq match {
            case Seq() => output
            case Seq(head, tail @ _*) => helper(tail, output.updated(head._2, 
                                                      output(head._2).updated(head._1, '0')))
        }
        helper(snake.body, bodylessBoard)
    }

    def render: Unit = {
        def prettyPrint: Unit = println("+" + "-" * width + "+")
        def helper(snake: Snake): Unit = {
            print("\u001b[2J")
            val board = snakeBoard(snake, emptyBoard)
            // --- --- ---
            prettyPrint
            board.foreach(n => println("|" + n.mkString("") + "|" ))
            prettyPrint
            // --- --- ---
            //Thread.sleep(500)
            helper(snake.next(io.StdIn.readChar))
        }
        helper(initSnake)
    }

}

object SnakeGame extends App {
    val game = new Game(10,30)
    game.render
}