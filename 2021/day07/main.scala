import scala.io.Source
object main {
    //Slope trick: https://codeforces.com/blog/entry/77298
    //Let f_p(x) = cost to move from position p to position x = |x-p|
    //We can represent f_p(x) using slope trick:
    // - For all x < p, f_p(x) = -x + p
    // - At x=p, the slope of f_p increases by 2 (goes from -1 to 1)
    //Let c(x) = total cost to move all crabs to position x
    //         = sum of f_p(x) over all positions p
    // - For all x < (minimum of all positions p), c(x) = -(number of positions)*x + (sum of all positions p)
    // - For all positions p, at x=p, the slope of c increases by 2
    def solvePart1(positions: Vector[Long]): Long = {
        var slope = -positions.length
        var yIntercept = positions.sum
        for (pos <- positions) {
            val fuelNeeded = slope*pos+yIntercept
            slope += 2
            //If we go from a negative to a non-negative slope,
            //that means this point is our minimum
            if (slope >= 0) {
                return fuelNeeded
            }
            yIntercept = fuelNeeded-slope*pos
        }
        throw new AssertionError("This point will never be reached!")
    }

    //Note: in part 2, I assume positions.min >= 0, so I don't check any negative positions
    //Let f_p(x) = cost to move from position p to position x = 1 + 2 + ... + |x-p|
    /*
     Here is a table representation of f_p:
      x  | p-4 | p-3 | p-2 | p-1 | p | p+1 | p+2 | p+3 | p+4
     --------------------------------------------------------
     f_p | 10  | 6   | 3   | 1   | 0 | 1   | 3   | 6   | 10
     Thus, the slope of f_p goes -4 -> -3 -> -2 -> -1 -> 1 -> 2 -> 3 -> 4
     More precisely, for a > 0, from x=p-a to x=p-a+1, the slope of f_p is -a
                            and from x=p+a-1 to x=p+a, the slope of f_p is a
     */
    // - From x=-1 to 0, f_p(x) = (-p-1)*x + (1 + 2 + ... + p)
    //   (Note that 1 + 2 + ... + p = p*(p+1)/2.)
    // - At x=p, slope of f_p increases by 2 (goes from -1 to 1)
    // - At every other x, slope of f_p increases by 1
    //Let c(x) = total cost to move all crabs to position x
    //         = sum of f_p(x) over all positions p
    // - From x=-1 to 0, c(x) = (-(sum of positions p)-(number of positions))*x + (sum of p*(p+1)/2 over all p)
    // - At every x, slope of c increases by (number of positions)
    // - For every position p, at x=p, slope of c increases by an extra 1
    //                                 to account for the -1 -> 1 slope change in f_p(x)
    def solvePart2(positions: Vector[Long]): Long = {
        val maxPos = positions.max
        var idx = 0

        var slope = -positions.sum-positions.length
        var yIntercept = positions.map(pos => (pos*(pos+1))/2).sum
        for (pos <- Range(0, maxPos.toInt+1, 1)) {
            val fuelNeeded = slope*pos+yIntercept
            slope += positions.length
            while ((idx < positions.length) && (positions(idx) == pos)) {
                slope += 1
                idx += 1
            }
            if (slope >= 0) {
                return fuelNeeded
            }
            yIntercept = fuelNeeded-slope*pos
        }
        throw new AssertionError("This point will never be reached!")
    }
        
    def main(args: Array[String]): Unit = {
        val puzzleInput = Source.fromFile(args(0)).getLines().toVector
        val positions = puzzleInput(0).split(",").map(_.toLong).toVector.sorted
        println(f"Part 1: ${solvePart1(positions)}")
        println(f"Part 2: ${solvePart2(positions)}")
    }
}