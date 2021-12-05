import scala.io.Source
import scala.collection.mutable.ArrayBuffer
import scala.util.control.Breaks._
object main {
    class Line(val x1: Long, val y1: Long, val x2: Long, val y2: Long)
    
    def parseLine(line: String): Line = {
        val s"$x1,$y1 -> $x2,$y2" = line
        new Line(x1.toLong, y1.toLong, x2.toLong, y2.toLong)
    }
    
    def gcd(a: Long, b: Long): Long = {
        if (a == 0) {
            return b
        }
        if (b == 0) {
            return a
        }
        
        var remainder = a
        var lastRemainder = b
        var lastLastRemainder = 0L
        while (remainder > 0) {
            lastLastRemainder = lastRemainder
            lastRemainder = remainder
            remainder = lastLastRemainder % lastRemainder
        }
        lastRemainder
    }

    def addPoints(numLines: Array[Array[Long]], ln: Line): Unit = {
        var stepX = ln.x2-ln.x1
        var stepY = ln.y2-ln.y1
        var gcdStep = gcd(Math.abs(stepX), Math.abs(stepY))
        stepX /= gcdStep
        stepY /= gcdStep

        var curX = ln.x1
        var curY = ln.y1
        numLines(curX.toInt)(curY.toInt) += 1
        while ((curX != ln.x2) || (curY != ln.y2)) {
            curX += stepX
            curY += stepY
            numLines(curX.toInt)(curY.toInt) += 1
        }
    }

    def countIntersections(numLines: Array[Array[Long]]): Long = {
        numLines.map(row => row.count(cell => cell > 1)).sum
    }
    
    def main(args: Array[String]): Unit = {
        val puzzleInput = Source.fromFile(args(0)).getLines().map(parseLine).toVector
        var maxX = 0
        var maxY = 0
        for (ln <- puzzleInput) {
            maxX = Math.max(maxX, Math.max(ln.x1, ln.x2).toInt)
            maxY = Math.max(maxY, Math.max(ln.y1, ln.y2).toInt)
        }

        val numLines = new Array[Array[Long]](maxX+1)
        for (i <- Range(0, maxX+1, 1)) {
            numLines(i) = Array.fill(maxY+1)(0)
        }

        for (ln <- puzzleInput) {
            if ((ln.x1 == ln.x2) || (ln.y1 == ln.y2)) {
                addPoints(numLines, ln)
            }
        }
        val part1Ans = countIntersections(numLines)
        System.out.println(f"Part 1: ${part1Ans}")
        
        for (ln <- puzzleInput) {
            if ((ln.x1 != ln.x2) && (ln.y1 != ln.y2)) {
                addPoints(numLines, ln)
            }
        }
        val part2Ans = countIntersections(numLines)
        System.out.println(f"Part 2: ${part2Ans}")
    }
}