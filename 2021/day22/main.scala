import scala.io.Source
import scala.collection.mutable._
import scala.util.control.Breaks._
//https://stackoverflow.com/a/9727679/3740708
object Int {
    def unapply(v: String) = try Some(v.toInt) catch { case _: NumberFormatException => None }
}
object main {
    val NUM_BITS = 64
    val SMALLBOUND = 50
    case class Command(val state: Boolean, val bounds: Vector[(Int, Int)])

    def parseLine(line: String): Command = {
        line match {
            case s"on x=${Int(xMin)}..${Int(xMax)},y=${Int(yMin)}..${Int(yMax)},z=${Int(zMin)}..${Int(zMax)}" => {
                Command(true, Vector((xMin, xMax), (yMin, yMax), (zMin, zMax)))
            }
            case s"off x=${Int(xMin)}..${Int(xMax)},y=${Int(yMin)}..${Int(yMax)},z=${Int(zMin)}..${Int(zMax)}" => {
                Command(false, Vector((xMin, xMax), (yMin, yMax), (zMin, zMax)))
            }
        }
    }

    def invVec(v: Vector[Int]): Map[Int, Int] = {
        var ans = Map[Int, Int]()
        for (i <- Range(0, v.length)) {
            ans += (v(i) -> i)
        }
        ans
    }
    
    def main(args: Array[String]): Unit = {
        val puzzleInput = Source.fromFile(args(0)).getLines().map(parseLine).toVector
        val allCoords = Vector.fill(3)(Set[Int]())
        for (comm <- puzzleInput) {
            for (i <- Range(0, comm.bounds.length)) {
                val (minB, maxB) = comm.bounds(i)
                allCoords(i) += minB
                allCoords(i) += maxB+1
            }
        }
        val sortedCoords = allCoords.map(_.toVector.sorted).toVector
        val coordMaps = sortedCoords.map(invVec).toVector
        
        var smallLocs = Array.fill(2*SMALLBOUND+1)(Array.fill(2*SMALLBOUND+1)(Array.fill(2*SMALLBOUND+1)(false)))
        var locs = Array.fill(sortedCoords(0).length)(Array.fill(sortedCoords(1).length)(Array.fill((sortedCoords(2).length+NUM_BITS-1)/NUM_BITS)(0L)))
        
        val adjustedComms = puzzleInput.map(comm => {
            Command(comm.state, comm.bounds.zipWithIndex.map({ case ((minB, maxB), i) => (coordMaps(i)(minB), coordMaps(i)(maxB+1)) }).toVector)
        }).toVector
        for (comm <- adjustedComms) {
            val (minX, maxX) = comm.bounds(0)
            val (minY, maxY) = comm.bounds(1)
            val (minZ, maxZ) = comm.bounds(2)
            for (x <- Range(minX, maxX)) {
                for (y <- Range(minY, maxY)) {
                    var z = minZ
                    while (z < maxZ) {
                        if (((z % NUM_BITS) == 0) && (z+NUM_BITS <= maxZ)) {
                            locs(x)(y)(z/NUM_BITS) = if (comm.state) { ~0L } else { 0L }
                            z += NUM_BITS
                        } else {
                            if (z+NUM_BITS-(z % NUM_BITS) <= maxZ) {
                                val relBits = ((1L << (NUM_BITS-(z % NUM_BITS)))-1L) << (z % NUM_BITS)
                                if (comm.state) locs(x)(y)(z/NUM_BITS) |= relBits
                                else locs(x)(y)(z/NUM_BITS) &= ~relBits
                                z += NUM_BITS-(z % NUM_BITS)
                            } else {
                                val relBits = ((1L << (maxZ-z))-1L) << (z % NUM_BITS)
                                if (comm.state) locs(x)(y)(z/NUM_BITS) |= relBits
                                else locs(x)(y)(z/NUM_BITS) &= ~relBits
                                z = maxZ
                            }
                        }
                    }
                }
            }
            val (tMinX, tMaxX) = (Math.max(-SMALLBOUND, sortedCoords(0)(minX)), Math.min(SMALLBOUND+1, sortedCoords(0)(maxX)))
            val (tMinY, tMaxY) = (Math.max(-SMALLBOUND, sortedCoords(1)(minY)), Math.min(SMALLBOUND+1, sortedCoords(1)(maxY)))
            val (tMinZ, tMaxZ) = (Math.max(-SMALLBOUND, sortedCoords(2)(minZ)), Math.min(SMALLBOUND+1, sortedCoords(2)(maxZ)))
            for (x <- Range(tMinX, tMaxX)) {
                for (y <- Range(tMinY, tMaxY)) {
                    for (z <- Range(tMinZ, tMaxZ)) {
                        smallLocs(x+SMALLBOUND)(y+SMALLBOUND)(z+SMALLBOUND) = comm.state
                    }
                }
            }
        }

        var part1Ans = 0L
        for (x <- Range(0, 2*SMALLBOUND+1)) {
            for (y <- Range(0, 2*SMALLBOUND+1)) {
                for (z <- Range(0, 2*SMALLBOUND+1)) {
                    if (smallLocs(x)(y)(z)) part1Ans += 1
                }
            }
        }
        println(f"Part 1: ${part1Ans}")
        
        var part2Ans = 0L
        for (i <- Range(0, sortedCoords(0).length-1)) {
            for (j <- Range(0, sortedCoords(1).length-1)) {
                val intermProduct = (sortedCoords(0)(i+1)-sortedCoords(0)(i)).toLong*(sortedCoords(1)(j+1)-sortedCoords(1)(j)).toLong
                var sumZs = 0L
                for (k <- Range(0, sortedCoords(2).length-1)) {
                    if (((locs(i)(j)(k/NUM_BITS) >> (k % NUM_BITS)) & 1L) == 1L) {
                        sumZs += (sortedCoords(2)(k+1)-sortedCoords(2)(k)).toLong
                    }
                }
                part2Ans += intermProduct*sumZs
            }
        }
        println(f"Part 2: ${part2Ans}")
    }
}
