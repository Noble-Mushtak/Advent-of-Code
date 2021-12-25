import scala.io.Source
import scala.collection.mutable._
import scala.util.control.Breaks._
object main {
    val EMPTY_CHAR = '.'
    
    def advanceDirection(state: Array[Array[Char]], dir: Char, dx: Int, dy: Int): Boolean = {
        var toMove = ArrayBuffer[(Int, Int)]()
        for (i <- Range(0, state.length)) {
            for (j <- Range(0, state(i).length)) {
                if ((state(i)(j) == dir) && (state((i+dy) % state.length)((j+dx) % state(i).length) == EMPTY_CHAR)) {
                    toMove += ((i, j))
                }
            }
        }
        for ((i, j) <- toMove) {
            state(i)(j) = EMPTY_CHAR
            state((i+dy) % state.length)((j+dx) % state(i).length) = dir
        }
        !toMove.isEmpty
    }

    def advanceState(state: Array[Array[Char]]): Boolean = {
        val moved1 = advanceDirection(state, '>', 1, 0)
        val moved2 = advanceDirection(state, 'v', 0, 1)
        moved1 || moved2
    }
    
    def main(args: Array[String]): Unit = {
        val puzzleInput = Source.fromFile(args(0)).getLines().map(_.toArray).toArray
        var numSteps = 1
        while (advanceState(puzzleInput)) {
            numSteps += 1
        }
        println(f"Part 1: ${numSteps}")
    }
}