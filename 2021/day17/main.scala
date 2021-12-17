import scala.io.Source
import scala.collection.mutable._
import scala.util.control.Breaks._
//https://stackoverflow.com/a/9727679/3740708
object Int {
    def unapply(v: String) = try Some(v.toInt) catch { case _: NumberFormatException => None }
}
object main {
    def main(args: Array[String]): Unit = {
        val puzzleInput = Source.fromFile(args(0)).getLines().toVector
        val s"target area: x=${Int(minX)}..${Int(maxX)}, y=${Int(minY)}..${Int(maxY)}" = puzzleInput(0)
        
        val part1Ans = -minY*(-minY-1)/2
        println(f"Part 1: ${part1Ans}")

        var part2Ans = 0
        for (xVel <- Range(1, maxX+1)) {
            for (yVel <- Range(minY, -minY)) {
                var curX = 0
                var curY = 0
                var curXVel = xVel
                var curYVel = yVel
                breakable { while (curY >= minY) {
                    if ((curX >= minX) && (curX <= maxX) && (curY >= minY) && (curY <= maxY)) {
                        part2Ans += 1
                        break()
                    }
                    curX += curXVel
                    curY += curYVel
                    if (curXVel > 0) curXVel -= 1
                    curYVel -= 1
                } }
            }
        }
        println(f"Part 2: ${part2Ans}")
    }
}