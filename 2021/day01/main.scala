import scala.io.Source
object main {
    def main(args: Array[String]): Unit = {
        val part1Input = Source.fromFile(args(0)).getLines().map(_.toLong).toList
        var prev = 1e18.toLong
        var ans = 0
        for (x <- part1Input) {
            if (x > prev) ans += 1;
            prev = x
        }
        System.out.println(f"Part 1: ${ans}");

        var prev3 = 1e18.toLong
        var prev2 = 1e18.toLong
        var prev1 = 1e18.toLong
        var ans2 = 0
        for (x <- part1Input) {
            if (x+prev1+prev2 > prev1+prev2+prev3) ans2 += 1;
            prev3 = prev2
            prev2 = prev1
            prev1 = x
        }
        System.out.println(f"Part 2: ${ans2}")
    }
}