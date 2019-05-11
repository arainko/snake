import scala.collection.immutable.{Vector => Vec}
import scala.util.Random

class Snake(headCord: (Int, Int), bodyCords: Seq[(Int, Int)], direction: String) {
    def body: Seq[(Int, Int)] = bodyCords
    def head: (Int, Int) = headCord
    def dir: String = direction
    def whole: Seq[(Int, Int)] = headCord +: bodyCords

    def next(input: Char, board: Vec[Vec[Char]]): Snake = {
        val newDir = input match {
            case 'a' if ("right" != direction) => "left"
            case 'w' if ("down" != direction) => "up"
            case 'd' if ("left" != direction) => "right"
            case 's' if ("up" != direction) => "down"
            case _ => direction
        }

        val newSnake = newDir match {
            case "left" => new Snake( (headCord._1-1, headCord._2), headCord +: bodyCords.init, newDir )
            case "up" => new Snake( (headCord._1, headCord._2-1), headCord +: bodyCords.init, newDir )
            case "right" => new Snake( (headCord._1+1, headCord._2), headCord +: bodyCords.init, newDir )
            case "down" => new Snake( (headCord._1, headCord._2+1), headCord +: bodyCords.init, newDir )
        }

        newSnake.head match {
            case (x, y) if (x == -1) => new Snake ((board.head.size-1, y), newSnake.body, newSnake.dir)
            case (x, y) if (x == board.head.size) => new Snake ( (0, y), newSnake.body, newSnake.dir)
            case (x, y) if (y == -1) => new Snake ((x, board.size-1), newSnake.body, newSnake.dir)
            case (x, y) if (y == board.size) => new Snake ((x, 0), newSnake.body, newSnake.dir)
            case _ => newSnake
        }
    }

    def appleCheck(apple: Apple): Snake = apple.pos match {
        case applePos if (headCord == applePos) => new Snake(headCord, bodyCords :+ headCord, direction)
        case _ => new Snake(headCord, bodyCords, direction)
    }

}

class Apple(posX: Int, posY: Int) {
    def x: Int = posX
    def y: Int = posY
    def pos: (Int, Int) = (posX, posY)

    def next(snake: Snake, board: Vec[Vec[Char]]): Apple = {
        val valueX = Random.nextInt(board.head.size)
        val valueY = Random.nextInt(board.size)
        val propableCords = (valueX, valueY)
        val wholeSnake = snake.whole

        wholeSnake.exists(n => n == propableCords) match {
            case true => next(snake, board)
            case false => new Apple(valueX, valueY)
        }
    }
}

class Game(h: Int, w: Int) {

    val height = h
    val width = w

    val initSnakeBody = Seq((14,5), (15,5), (16,5))
    val initSnake = new Snake( (13,5), initSnakeBody, "left")
    val emptyBoard = initialBoard(height, width)
    val initApple = new Apple(0,0)

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

    private def snakeBoard(snake: Snake, apple: Apple, board: Vec[Vec[Char]]): Vec[Vec[Char]] = {
        val bodylessBoard = board.updated(snake.head._2,
                            board(snake.head._2).updated(snake.head._1, 'X'))
        
        val appleBoard = bodylessBoard.updated(apple.y,
                         bodylessBoard(apple.y).updated(apple.x, '*'))

        def helper(snakePosSeq: Seq[(Int, Int)], output: Vec[Vec[Char]]): Vec[Vec[Char]] = snakePosSeq match {
            case Seq(single) => output
            case Seq(head, tail @ _*) => helper(tail, output.updated(head._2, 
                                                      output(head._2).updated(head._1, '0')))
        }
        helper(snake.body, appleBoard)
    }

    def render: Unit = {
        def prettyPrint: Unit = println("+" + "-" * width + "+")

        def helper(snake: Snake, apple: Apple): Unit = {
            print("\u001b[2J")
            val board = snakeBoard(snake, apple, emptyBoard)
            // --- --- ---
                prettyPrint
                board.foreach(n => println("|" + n.mkString("") + "|" ))
                prettyPrint
            // --- --- ---
            val input = io.StdIn.readChar
            val appleCheckedSnake = snake.next(input, emptyBoard).appleCheck(apple)
            val nextApple = apple.next(appleCheckedSnake, emptyBoard)

            if (appleCheckedSnake.head == apple.pos) helper(appleCheckedSnake, nextApple)
            else helper(appleCheckedSnake, apple)
        }
        helper(initSnake, initApple)
    }

}

object SnakeGame extends App {
    val game = new Game(10,30)
    game.render
}