import scala.io.Source
import scala.collection.mutable._
import scala.util.control.Breaks._
object main {
    val perm3 = Vector(Vector(0,1,2),Vector(0,2,1),Vector(1,0,2),Vector(1,2,0),Vector(2,0,1),Vector(2,1,0))
    val signs = Vector(Vector(1,1,1),Vector(1,1,-1),Vector(1,-1,1),Vector(-1,1,1),Vector(1,-1,-1),Vector(-1,1,-1),Vector(-1,-1,1),Vector(-1,-1,-1))

    def calcSum(p1: Vector[Int], p2: Vector[Int]): Vector[Int] = {
        Vector(p1(0)+p2(0), p1(1)+p2(1), p1(2)+p2(2))
    }

    def calcDiff(p1: Vector[Int], p2: Vector[Int]): Vector[Int] = {
        Vector(p1(0)-p2(0), p1(1)-p2(1), p1(2)-p2(2))
    }

    def manhattanDistance(p1: Vector[Int], p2: Vector[Int]): Int = {
        Math.abs(p1(0)-p2(0))+Math.abs(p1(1)-p2(1))+Math.abs(p1(2)-p2(2))
    }

    def applyOrient(a: Vector[Int], perm: Vector[Int], sgn: Vector[Int]): Vector[Int] = {
        Vector(sgn(0)*a(perm(0)), sgn(1)*a(perm(1)), sgn(2)*a(perm(2)))
    }

    def composeOrients(perm1: Vector[Int], sgn1: Vector[Int], perm2: Vector[Int], sgn2: Vector[Int]): (Vector[Int], Vector[Int]) = {
        //a''(i) = sgn2(i)*a'(perm2(i)) = sgn2(i)*sgn1(perm2(i))*a(perm1(perm2(i)))
        (Vector(perm1(perm2(0)), perm1(perm2(1)), perm1(perm2(2))), Vector(sgn2(0)*sgn1(perm2(0)), sgn2(1)*sgn1(perm2(1)), sgn2(2)*sgn1(perm2(2))))
    }

    def main(args: Array[String]): Unit = {
        val puzzleInput = Source.fromFile(args(0)).mkString.split("\n\n").map(_.split("\n").filter(_.contains(",")).map(_.split(",").map(_.toInt).toVector).toVector).toVector
        val N = puzzleInput.length
        
        var distances: Array[Array[Option[(Vector[Int], Vector[Int], Vector[Int])]]] = Array.fill(N)(Array.fill(N)(None))
        for (i <- Range(0, N)) {
            val pointsISorted = puzzleInput(i).sortBy(_(0))
            var diffMap: Map[Vector[Int], (Int, Int)] = Map()
            for (x2 <- Range(0, puzzleInput(i).length)) {
                for (x1 <- Range(0, x2)) {
                    diffMap += (calcDiff(pointsISorted(x1), pointsISorted(x2)) -> (x1, x2))
                }
            }
            for (j <- Range(0, N)) {
                if (i != j) {
                    breakable { for (perm <- perm3) {
                        for (sgn <- signs) {
                            val pointsJSorted = puzzleInput(j).map(applyOrient(_, perm, sgn)).sortBy(_(0))
                            var pointMap: Map[Int, Int] = Map()
                            val handlePoint = (x1: Int, i1: Int) => {
                                pointMap.get(x1) match {
                                    case None => {
                                        pointMap += (x1 -> i1)
                                        true
                                    }
                                    case Some(i2) => i1 == i2
                                }
                            }
                            var good = true
                            breakable { for (x2 <- Range(1, pointsJSorted.length)) {
                                for (x1 <- Range(0, x2)) {
                                    val curDiff = calcDiff(pointsJSorted(x1), pointsJSorted(x2))
                                    diffMap.get(curDiff) match {
                                        case None => ()
                                        case Some((i2, j2)) => {
                                            if (!handlePoint(x1, i2) || !handlePoint(x2, j2)) {
                                                good = false
                                                break()
                                            }
                                        }
                                    }
                                }
                            } }
                            if (good && (pointMap.size >= 12)) {
                                val (x1, i1) = pointMap.iterator.next()
                                val jLoc = calcDiff(pointsISorted(i1), pointsJSorted(x1))
                                distances(i)(j) = Some((jLoc, perm, sgn))
                                break()
                            }
                        } }
                    }
                }
            }
        }

        var scannerMap = Array.fill(N)(Vector(0,0,0))
        var beaconSet = puzzleInput(0).toSet
        var bfsQ = Queue[(Int, Vector[Int], Vector[Int], Vector[Int])]()
        bfsQ.enqueue((0, Vector(0,0,0), perm3(0), signs(0)))
        var addedToQ = Array.fill(N)(false)
        addedToQ(0) = true
        while (!bfsQ.isEmpty) {
            val (curV, curLoc, perm, sgn) = bfsQ.dequeue()
            scannerMap(curV) = curLoc
            for (i <- Range(0, N)) {
                if (!addedToQ(i)) {
                    distances(curV)(i) match {
                        case None => ()
                        case Some((offset, perm2, sgn2)) => {
                            val (newPerm, newSgn) = composeOrients(perm2, sgn2, perm, sgn)
                            val newLoc = calcSum(curLoc, applyOrient(offset, perm, sgn))
                            for (point <- puzzleInput(i)) {
                                beaconSet += calcSum(newLoc, applyOrient(point, newPerm, newSgn))
                            }
                            bfsQ.enqueue((i, newLoc, newPerm, newSgn))
                            addedToQ(i) = true
                        }
                    }
                }
            }
        }
        println(f"Part 1: ${beaconSet.size}")

        var maxDist = 0
        for (i <- Range(0, N)) {
            for (j <- Range(0, N)) {
                maxDist = Math.max(maxDist, manhattanDistance(scannerMap(i), scannerMap(j)))
            }
        }
        println(f"Part 2: ${maxDist}")
    }
}