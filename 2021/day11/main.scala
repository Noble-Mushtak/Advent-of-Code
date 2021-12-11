import scala.io.Source
import scala.collection.mutable._
import scala.util.control.Breaks._
object main {
    val neighbors = Vector((0, 1), (1, 0), (0, -1), (-1, 0), (1, 1), (-1, -1), (1, -1), (-1, 1))

    def advancePointInState(state: Array[Array[Long]], bfsQ: Queue[(Int, Int)], x: Int, y: Int): Unit = {
        state(x)(y) += 1
        if (state(x)(y) > 9) {
            state(x)(y) = 0
            bfsQ.enqueue((x, y))
        }
    }
    
    def advanceState(state: Array[Array[Long]]): Long = {
        var bfsQ = Queue[(Int, Int)]()
        for (i <- Range(0, state.length, 1)) {
            for (j <- Range(0, state(i).length, 1)) {
                advancePointInState(state, bfsQ, i, j)
            }
        }

        var ans = 0
        while (!bfsQ.isEmpty) {
            val (curX, curY) = bfsQ.dequeue()
            ans += 1
            for ((dx, dy) <- neighbors) {
                val (newX, newY) = (curX+dx, curY+dy)
                try {
                    if (state(newX)(newY) > 0) {
                        advancePointInState(state, bfsQ, newX, newY)
                    }
                } catch {
                    case e: IndexOutOfBoundsException => ()
                }
            }
        }
        ans
    }
       
    def main(args: Array[String]): Unit = {
        val puzzleInput = Source.fromFile(args(0)).getLines().map(_.toArray.map(_.toString.toLong)).toArray
        var part1Ans = 0L
        var part2Ans = 0
        var ansFound = false
        var numFlashes = 0L
        while (!ansFound || (part2Ans < 100)) {
            numFlashes = advanceState(puzzleInput)
            if (part2Ans < 100) {
                part1Ans += numFlashes
            }
            if (!ansFound) {
                part2Ans += 1
                ansFound = (numFlashes == puzzleInput.length*puzzleInput(0).length)
            }
        }
        println(f"Part 1: ${part1Ans}")
        println(f"Part 2: ${part2Ans}")
    }
}