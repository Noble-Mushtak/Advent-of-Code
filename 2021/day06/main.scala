import scala.io.Source
import scala.collection.mutable.ArrayBuffer
import scala.util.control.Breaks._
object main {
    val CYCLE_TIME = 7
    val RESET_TIME = 2
    val TOTAL_TIME = CYCLE_TIME+RESET_TIME

    def advanceCounts(fishCounts: Array[Long]): Array[Long] = {
        var answer = Array.fill(TOTAL_TIME)(0L)
        for (i <- Range(0, TOTAL_TIME, 1)) {
            if (i < TOTAL_TIME-1) {
                answer(i) = fishCounts(i+1)
                if (i+1 == CYCLE_TIME) {
                    answer(i) += fishCounts(0)
                }
            } else {
                answer(i) = fishCounts(0)
            }
        }
        answer
    }

    def simulateCounts(fishCounts: Array[Long], numDays: Long): Array[Long] = {
        var answer = fishCounts
        for (i <- Range(0, numDays.toInt, 1)) {
            answer = advanceCounts(answer)
        }
        answer
    }
    
    def main(args: Array[String]): Unit = {
        val puzzleInput = Source.fromFile(args(0)).getLines().toVector
        val initialFish = puzzleInput(0).split(",").map(_.toLong).toVector
        var initFishCounts = Array.fill(TOTAL_TIME)(0L)
        for (num <- initialFish) {
            initFishCounts(num.toInt) += 1L
        }
        
        val newFishCounts1 = simulateCounts(initFishCounts, 80L)
        println(f"Part 1: ${newFishCounts1.sum}")
        val newFishCounts2 = simulateCounts(initFishCounts, 256L)
        println(f"Part 2: ${newFishCounts2.sum}")
    }
}