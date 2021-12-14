import scala.io.Source
import scala.collection.mutable._
import scala.util.control.Breaks._
object main {
    val NUM_LETTERS = 26
    val NUM_PAIRS = NUM_LETTERS*NUM_LETTERS

    def hashPair(a: Char, b: Char): Int = {
        NUM_LETTERS*(a.toInt-'A'.toInt)+(b.toInt-'A'.toInt)
    }

    def performStep(state: Array[Long], rules: Array[Option[(Int, Int)]]): Array[Long] = {
        val newState = Array.fill(NUM_PAIRS)(0L)
        for (pr <- Range(0, NUM_PAIRS)) {
            rules(pr) match {
                case None => newState(pr) += state(pr)
                case Some((np1, np2)) => {
                    newState(np1) += state(pr)
                    newState(np2) += state(pr)
                }
            }
        }
        newState
    }

    def calcScore(state: Array[Long]): Long = {
        val freqs = Array.fill(NUM_LETTERS)(0L)
        for (pr <- Range(0, NUM_PAIRS)) {
            freqs(pr/NUM_LETTERS) += state(pr)
            freqs(pr % NUM_LETTERS) += state(pr)
        }
        for (l <- Range(0, NUM_LETTERS)) {
            freqs(l) = (freqs(l)+1)/2
        }
        freqs.max-freqs.filter(x => x > 0).min
    }
    
    def main(args: Array[String]): Unit = {
        val puzzleInput = Source.fromFile(args(0)).getLines().toVector
        val initString = puzzleInput(0)
        val initArray = Array.fill(NUM_PAIRS)(0L)
        for (i <- Range(0, initString.length-1)) {
            initArray(hashPair(initString(i), initString(i+1))) += 1
        }

        val rules = Array.fill(NUM_PAIRS)(None: Option[(Int, Int)])
        for (i <- Range(2, puzzleInput.length)) {
            val s"$pair -> $newchar" = puzzleInput(i)
            val oldPair = hashPair(pair(0), pair(1))
            val newPair1 = hashPair(pair(0), newchar(0))
            val newPair2 = hashPair(newchar(0), pair(1))
            assert(rules(oldPair) == None)
            rules(oldPair) = Some((newPair1, newPair2))
        }

        var curArray = initArray
        for (i <- Range(0, 10)) {
            curArray = performStep(curArray, rules)
        }
        println(f"Part 1: ${calcScore(curArray)}")
        for (i <- Range(0, 30)) {
            curArray = performStep(curArray, rules)
        }
        println(f"Part 2: ${calcScore(curArray)}")
    }
}