//Some code which solves a different problem slightly inspired by today's AoC,
//an extension of the problem in https://open.kattis.com/problems/itsamodmodmodmodworld
import scala.io.Source
import scala.collection.mutable._
import scala.util.control.Breaks._
//https://stackoverflow.com/a/9727679/3740708
object Long {
    def unapply(v: String) = try Some(v.toLong) catch { case _: NumberFormatException => None }
}
object main {
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
        while (remainder > 0L) {
            lastLastRemainder = lastRemainder
            lastRemainder = remainder
            remainder = lastLastRemainder % lastRemainder
        }
        lastRemainder
    }
    
    def inverse(a: Long, b: Long): Long = {
        var lastRemainder = a
        var lastInverse = 1L
        var remainder = b
        var inverse = 0L
        while (remainder > 0L) {
            val quotient = lastRemainder/remainder
            val temp = remainder
            remainder = lastRemainder % remainder
            lastRemainder = temp
            val temp2 = inverse
            inverse = lastInverse-quotient*inverse
            lastInverse = temp2
        }
        if (lastInverse < 0L) lastInverse += b
        lastInverse
    }

    //Given d=gcd(P, Q), calculate sum of i=1 to N of floor((Pi+C)/Q)
    def solveModModHelp(d: Long, Parg: Long, Qarg: Long, Carg: Long, Narg: Long): Long = {
        var P = Parg
        var Q = Qarg
        var N = Narg
        var C = Carg
        var tot = 0L
        var sgn = 1L
        while (true) {
            tot += sgn*(P/Q)*N*(N+1)/2
            if ((C % Q) >= 0) tot += sgn*(C/Q)*N
            else tot += sgn*(C/Q-1)*N
            P %= Q
            C %= Q
            if (C < 0) C += Q
            if (P == 0) return tot
            val maxTerm = (P*N+C)/Q
            var numBads = 0L
            if ((C % d) == 0L) {
                numBads = maxTerm/(P/d)
                val theBad = (inverse(Q/d, P/d)*(C/d)) % (P/d)
                if ((theBad != 0) && ((maxTerm % (P/d)) >= theBad)) numBads += 1
            }
            tot += sgn*(N*maxTerm + numBads)
            N = maxTerm
            val tmp = P
            P = Q
            Q = tmp
            sgn *= -1
            C *= -1
        }
        throw new AssertionError("The end of this function is never reached")
    }
    //Calculate sum of i=1 to N of (Pi+C) mod Q
    def solveModMod(P: Long, Q: Long, C: Long, N: Long): Long = {
        val d = gcd(P, Q)
        P*N*(N+1)/2 + C*N - Q*solveModModHelp(d, P, Q, C, N)
    }

    def solveModModNaive(P: Long, Q: Long, C: Long, N: Long): Long = {
        var ans = 0L
        for (i <- Range(1, N.toInt+1)) {
            ans += (P*i+C) % Q
        }
        ans
    }
    
    def main(args: Array[String]): Unit = {
        val puzzleInput = Source.fromFile(args(0)).getLines().toVector
        var s"${Long(p)} ${Long(q)} ${Long(c)} ${Long(n)}" = puzzleInput(0)
        assert(solveModMod(p, q, c, n) == solveModModNaive(p, q, c, n))
        println("Assert succeeded!")
    }
}