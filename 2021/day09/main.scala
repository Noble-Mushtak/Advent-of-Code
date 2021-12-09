import scala.io.Source
import scala.collection.mutable._
import scala.util.control.Breaks._
object main {
    val neighbors = Vector((0, 1), (1, 0), (0, -1), (-1, 0))
    
    def main(args: Array[String]): Unit = {
        val puzzleInput = Source.fromFile(args(0)).getLines().map(_.toVector.map(_.toString.toLong)).toVector
        var part1Ans = 0L;
        var lowPoints = new ArrayBuffer[(Int, Int)]()
        for (i <- Range(0, puzzleInput.length, 1)) {
            for (j <- Range(0, puzzleInput(i).length, 1)) {
                val curHeight = puzzleInput(i)(j);
                var minHeight = curHeight+1;
                for ((dx, dy) <- neighbors) {
                    try {
                        minHeight = Math.min(minHeight, puzzleInput(i+dx)(j+dy))
                    } catch {
                        case e: IndexOutOfBoundsException => ()
                    }
                }
                if (curHeight < minHeight) {
                    part1Ans += curHeight+1
                    lowPoints += ((i, j))
                }
            }
        }
        println(f"Part 1: ${part1Ans}")

        var basinSizes = Array(0, 0, 0)
        for (lowPoint <- lowPoints) {
            var basinSize = 0
            var vis = Set[(Int, Int)]()
            vis += lowPoint
            var curQ = Queue[(Int, Int)]()
            curQ.enqueue(lowPoint)
            while (!curQ.isEmpty) {
                val (x, y) = curQ.dequeue()
                basinSize += 1
                
                for ((dx, dy) <- neighbors) {
                    if (!vis.contains((x+dx, y+dy))) {
                        try {
                            val curHeight = puzzleInput(x+dx)(y+dy)
                            if (curHeight != 9) {
                                vis += ((x+dx, y+dy))
                                curQ.enqueue((x+dx, y+dy))
                            }
                        } catch {
                            case e: IndexOutOfBoundsException => ()
                        }
                    }
                }
            }

            for (i <- Range(basinSizes.length-1, -1, -1)) {
                if (basinSize > basinSizes(i)) {
                    val tmp = basinSizes(i)
                    basinSizes(i) = basinSize
                    basinSize = tmp
                }
            }
        }
        println(f"Part 2: ${basinSizes.product}")
    }
}