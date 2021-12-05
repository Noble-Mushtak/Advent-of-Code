import scala.io.Source
import scala.collection.mutable.ArrayBuffer
object main {
    def countBitFreqs(nums: Seq[String], j: Int): Array[Int] = {
        val numBits = nums(0).length
        val cnts = new Array[Int](2)
        for (i <- Range(0, nums.length, 1)) {
            if (nums(i)(j) == '0') cnts(0) += 1
            else cnts(1) += 1
        }
        cnts
    }

    def filterBits(nums: Seq[String], filterFunc: (Int, Int) => Boolean, preferredBit: Char): String = {
        val numBits = nums(0).length
        var curNums = nums
        for (j <- Range(0, numBits, 1)) {
            val cnts = countBitFreqs(curNums, j)
            val curBit = if (filterFunc(cnts(0), cnts(1))) {
                '0'
            } else if (filterFunc(cnts(1), cnts(0))) {
                '1'
            } else {
                preferredBit
            }
            var newNums = new ArrayBuffer[String]
            for (num <- curNums) {
                if (num(j) == curBit) {
                    newNums += num
                }
            }
            curNums = newNums.toSeq
            if (curNums.length == 1) {
                return curNums(0)
            } else if (curNums.length == 0) {
                throw new IllegalArgumentException("No answer found!")
            }
        }
        if (curNums.length == 0) {
            throw new IllegalArgumentException("No answer found!")
        } else {
            throw new IllegalArgumentException("Multiple answers found: ${curNums}")
        }
    }

    def main(args: Array[String]): Unit = {
        val puzzleInput = Source.fromFile(args(0)).getLines().toList
        val numBits = puzzleInput(0).length
        var gammaRate = 0
        var epsilonRate = 0
        for (j <- Range(0, numBits, 1)) {
            gammaRate <<= 1
            epsilonRate <<= 1
            val cnts = countBitFreqs(puzzleInput, j)
            if (cnts(0) < cnts(1)) {
                gammaRate |= 1
            } else {
                epsilonRate |= 1
            }
        }
        System.out.println(f"Part 1: ${gammaRate*epsilonRate}");

        val oxygenRate = Integer.parseInt(filterBits(puzzleInput, (cnt0: Int, cnt1: Int) => cnt0 > cnt1, '1'), 2)
        val co2Rate = Integer.parseInt(filterBits(puzzleInput, (cnt0: Int, cnt1: Int) => cnt0 < cnt1, '0'), 2)
        System.out.println(f"Part 2: ${oxygenRate*co2Rate}");
    }
}