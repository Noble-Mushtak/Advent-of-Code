//https://www.reddit.com/r/adventofcode/comments/rgc74r/2021_day_14_part_2_extend_exponentially_harder/
import scala.io.Source
import scala.collection.mutable._
import scala.util.control.Breaks._
object main {
    val NUM_LETTERS = 26
    val NUM_PAIRS = NUM_LETTERS*NUM_LETTERS
    val MOD = 1000000007L

    //Implement extended euclidean algorithm for modular inverse:
    def inverse(a: Long): Long = {
        var lastRemainder = a
        var lastInverse = 1L
        var remainder = MOD
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
        if (lastInverse < 0L) lastInverse += MOD
        lastInverse
    }
    val inverseTwo = inverse(2)

    //Represent each pair of chars as an integer in the interval [0, NUM_PAIRS)
    def hashPair(a: Char, b: Char): Int = {
        NUM_LETTERS*(a.toInt-'A'.toInt)+(b.toInt-'A'.toInt)
    }

    //Given the rules, output a NUM_PAIRS x NUM_PAIRS matrix
    //For every 0 <= i < NUM_PAIRS,
    //matrix(i) is a vector representing the frequencies of each pair
    //after applying the rules to the two-letter string containing the pair represented by the number i
    def buildMatrix(rules: Array[Option[(Int, Int)]]): Array[Array[Long]] = {
        val matrix = Array.fill(NUM_PAIRS)(Array.fill(NUM_PAIRS)(0L))
        for (i <- Range(0, NUM_PAIRS)) {
            rules(i) match {
                case None => matrix(i)(i) = 1L
                case Some((x, y)) => {
                    matrix(i)(x) = 1L
                    matrix(i)(y) = 1L
                }
            }
        }
        matrix
    }

    //Multiply matrix by vector according to matrix-vector multiplication rules
    def applyMatrix(matrix: Array[Array[Long]], vector: Array[Long]): Array[Long] = {
        Range(0, NUM_PAIRS).map(pr => Range(0, NUM_PAIRS).map(i => (vector(i)*matrix(i)(pr)) % MOD).sum % MOD).toArray
    }

    //Square the matrix according to matrix-matrix multiplication rules
    def squareMatrix(matrix: Array[Array[Long]]): Array[Array[Long]] = {
        Range(0, NUM_PAIRS).map(i => applyMatrix(matrix, matrix(i))).toArray
    }

    //Given a vector representing the initial frequencies of each pair in the string
    //and the rules for how the string changes after each step,
    //output a new vector representing the final frequencies of each pair in the string
    //after numSteps steps
    def performSteps(state: Array[Long], rules: Array[Option[(Int, Int)]], numSteps: Long): Array[Long] = {
        val ruleMatrix = buildMatrix(rules)
        var curMatrix = ruleMatrix
        var curState = state
        var stepsLeft = numSteps
        while (stepsLeft > 0) {
            if (stepsLeft % 2 == 1) {
                curState = applyMatrix(curMatrix, curState)
            }
            curMatrix = squareMatrix(curMatrix)
            stepsLeft >>= 1
        }
        curState
    }

    //Given a vector representing the frequency of every pair in the string,
    //calculate the frequency of every letter.
    //specialLets is a set of the first and last character,
    //so we can account for the fact that their appearance in the first/last pairs
    //is counted only once, instead of twice like all the other appearances of letters.
    def calcLetterFreqs(state: Array[Long], specialLets: Set[Int]): Array[Long] = {
        val freqs = Array.fill(NUM_LETTERS)(0L)
        for (pr <- Range(0, NUM_PAIRS)) {
            freqs(pr/NUM_LETTERS) += state(pr)
            if (freqs(pr/NUM_LETTERS) >= MOD) freqs(pr/NUM_LETTERS) -= MOD
            freqs(pr % NUM_LETTERS) += state(pr)
            if (freqs(pr % NUM_LETTERS) >= MOD) freqs(pr % NUM_LETTERS) -= MOD
        }
        for (l <- Range(0, NUM_LETTERS)) {
            val addOne = if (specialLets.contains(l)) { 1 } else { 0 }
            freqs(l) = ((freqs(l)+addOne)*inverseTwo) % MOD
        }
        freqs
    }

    //Given the frequency of every letter,
    //find the letters of minimum and maximum frequency
    def extremeLetters(freqs: Array[Long]): (Int, Int) = {
        var maxLetter = 0
        var minLetter = 0
        for (i <- Range(0, NUM_LETTERS)) {
            if (freqs(i) > freqs(maxLetter)) maxLetter = i
            //Make sure freqs(minLetter) is the minimum _positive_ frequency:
            if ((freqs(i) > 0) && ((freqs(minLetter) == 0) || (freqs(i) < freqs(minLetter)))) minLetter = i
        }
        (minLetter, maxLetter)
    }

    //Given the frequency of every letter
    //and the letters of minimum and maximum frequency,
    //calculate the final answer
    def calcScore(freqs: Array[Long], minLetter: Int, maxLetter: Int): Long = {
        var ans = freqs(maxLetter)-freqs(minLetter)
        if (ans < 0) ans += MOD
        ans
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

        val specialLets = Set(initString(0).toInt-'A'.toInt, initString(initString.length-1).toInt-'A'.toInt)
        val part1State = performSteps(initArray, rules, 10)
        val part1Freqs = calcLetterFreqs(part1State, specialLets)

        //Find letters with maximum and minimum frequency based on part 1
        val (minLetter, maxLetter) = extremeLetters(part1Freqs)
        
        println(f"Part 1: ${calcScore(part1Freqs, minLetter, maxLetter)}")
        println(f"Part 2: ${calcScore(calcLetterFreqs(performSteps(initArray, rules, 40), specialLets), minLetter, maxLetter)}")
        println(f"Extension: ${calcScore(calcLetterFreqs(performSteps(initArray, rules, 1000000000), specialLets), minLetter, maxLetter)}")
    }
}