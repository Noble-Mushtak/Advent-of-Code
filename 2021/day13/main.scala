import scala.io.Source
import scala.collection.mutable._
import scala.util.control.Breaks._
object main {
    sealed trait Fold
    case class FoldX(val x: Int) extends Fold
    case class FoldY(val y: Int) extends Fold

    def processFold(array: Array[Array[Boolean]], fold: Fold): Array[Array[Boolean]] = {
        val (dimen1, dimen2) = fold match {
            case FoldX(x) => (array.length, x)
            case FoldY(y) => (y, array(0).length)
        }
        var newArray = Array.fill(dimen1)(Array.fill(dimen2)(false))
        for (i <- Range(0, dimen1)) {
            for (j <- Range(0, dimen2)) {
                newArray(i)(j) = array(i)(j)
                val (reflI: Int, reflJ: Int) = fold match {
                    case FoldX(x) => (i, 2*x-j)
                    case FoldY(y) => (2*y-i, j)
                }
                newArray(i)(j) ||= array(reflI)(reflJ)
            }
        }
        newArray
    }

    def countTrues(array: Array[Array[Boolean]]): Int = {
        array.map(row => row.map(b => if (b) { 1 } else { 0 }).sum).sum
    }
    
    def main(args: Array[String]): Unit = {
        val puzzleInput = Source.fromFile(args(0)).getLines().toVector
        val points = ArrayBuffer[(Int, Int)]()
        var maxX = 0
        var maxY = 0
        val folds = ArrayBuffer[Fold]()
        for (line <- puzzleInput) {
            line match {
                case s"$x,$y" => {
                    maxX = Math.max(maxX, x.toInt)
                    maxY = Math.max(maxY, y.toInt)
                    points += ((x.toInt, y.toInt))
                }
                case s"fold along x=$x" => folds += FoldX(x.toInt)
                case s"fold along y=$y" => folds += FoldY(y.toInt)
                case _ => ()
            }
        }

        var pointArray = Array.fill(maxY+1)(Array.fill(maxX+1)(false))
        for ((x, y) <- points) pointArray(y)(x) = true
        val newPointArray = processFold(pointArray, folds(0))
        println(f"Part 1: ${countTrues(newPointArray)}")

        val finalPointArray = Range(1, folds.length).foldLeft(newPointArray)((pointArray, idx) => processFold(pointArray, folds(idx)))
        println("Part 2:")
        for (i <- Range(0, finalPointArray.length)) {
            for (j <- Range(0, finalPointArray(i).length)) {
                print(if (finalPointArray(i)(j)) { '#' } else { '.' })
            }
            println()
        }
    }
}