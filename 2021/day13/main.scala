import scala.io.Source
import scala.collection.mutable.ArrayBuffer
import scala.util.control.Breaks._
object main {
    sealed trait Fold
    case class FoldX(val x: Int) extends Fold
    case class FoldY(val y: Int) extends Fold

    def processFold(points: Set[(Int, Int)], fold: Fold): Set[(Int, Int)] = {
        var newPoints = Set[(Int, Int)]()
        for ((i, j) <- points) {
            newPoints += (fold match {
                case FoldX(x) => (Math.min(i, 2*x-i), j)
                case FoldY(y) => (i, Math.min(j, 2*y-j))
            })
        }
        newPoints
    }
    
    def main(args: Array[String]): Unit = {
        val puzzleInput = Source.fromFile(args(0)).getLines().toVector
        val points = ArrayBuffer[(Int, Int)]()
        val folds = ArrayBuffer[Fold]()
        for (line <- puzzleInput) {
            line match {
                case s"$x,$y" => points += ((x.toInt, y.toInt))
                case s"fold along x=$x" => folds += FoldX(x.toInt)
                case s"fold along y=$y" => folds += FoldY(y.toInt)
                case _ => ()
            }
        }

        val newPoints = processFold(points.toSet, folds(0))
        println(f"Part 1: ${newPoints.size}")

        val finalPoints = Range(1, folds.length).foldLeft(newPoints)((points, idx) => processFold(points, folds(idx)))
        val maxX = finalPoints.map({ case (x, _) => x }).max
        val maxY = finalPoints.map({ case (_, y) => y }).max
        println("Part 2:")
        for (i <- Range(0, maxY+1)) {
            for (j <- Range(0, maxX+1)) {
                print(if (finalPoints.contains((j, i))) { '#' } else { '.' })
            }
            println()
        }
    }
}