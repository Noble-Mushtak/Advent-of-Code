import scala.io.Source
import scala.collection.mutable.ArrayBuffer
import scala.util.control.Breaks._
object main {
    val NUM_ROWS = 5

    def calcScores(callingOrder: Vector[Long], boards: Vector[Vector[Vector[Long]]]): Vector[Long] = {
        var markedElems = new ArrayBuffer[ArrayBuffer[ArrayBuffer[Boolean]]]()
        for (b <- boards) {
            var markedElem = new ArrayBuffer[ArrayBuffer[Boolean]]()
            for (i <- Range(0, b.length, 1)) {
                markedElem += ArrayBuffer.fill(b(i).length)(false)
            }
            markedElems += markedElem
        }

        var hasWon = Array.fill(boards.length)(false)
        var winningScores = new ArrayBuffer[Long]
        for (num <- callingOrder) {
            for (k <- Range(0, boards.length, 1)) {
                if (!hasWon(k)) {
                    breakable { for (i <- Range(0, boards(k).length, 1)) {
                        for (j <- Range(0, boards(k)(i).length, 1)) {
                            if (boards(k)(i)(j) == num) {
                                markedElems(k)(i)(j) = true

                                var winning = true
                                breakable {for (j2 <- Range(0, boards(k)(i).length, 1)) {
                                    if (!markedElems(k)(i)(j2)) {
                                        winning = false
                                        break()
                                    }
                                } }

                                if (!winning) {
                                    winning = true
                                    breakable { for (i2 <- Range(0, boards(k).length, 1)) {
                                        if (!markedElems(k)(i2)(j)) {
                                            winning = false
                                            break()
                                        }
                                    } }
                                }

                                if (winning) {
                                    hasWon(k) = true
                                    winningScores += calcScore(boards(k), markedElems(k), num)
                                    break()
                                }
                            }
                        }
                    } }
                }
            }
        }
        winningScores.toVector
    }

    def calcScore(board: Vector[Vector[Long]], markedElem: ArrayBuffer[ArrayBuffer[Boolean]], num: Long): Long = {
        var sum = 0L
        for (i <- Range(0, board.length, 1)) {
            for (j <- Range(0, board(i).length, 1)) {
                if (!markedElem(i)(j)) {
                    sum += board(i)(j)
                }
            }
        }
        sum*num
    }
    
    def main(args: Array[String]): Unit = {
        val puzzleInput = Source.fromFile(args(0)).getLines().toVector
        val callingOrder = puzzleInput(0).split(",").map(_.toLong).toVector
        var boards = new ArrayBuffer[Vector[Vector[Long]]]()
        for (i <- Range(2, puzzleInput.length, NUM_ROWS+1)) {
            var board = new ArrayBuffer[Vector[Long]]()
            for (j <- Range(0, NUM_ROWS, 1)) {
                val curRow = puzzleInput(i+j).trim().split("\\s+").map(_.toLong).toVector
                board += curRow
            }
            boards += board.toVector
        }
        val winningScores = calcScores(callingOrder, boards.toVector)
        System.out.println(f"Part 1: ${winningScores(0)}")
        System.out.println(f"Part 2: ${winningScores(winningScores.length-1)}")
    }
}