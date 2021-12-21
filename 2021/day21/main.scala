import scala.io.Source
import scala.collection.mutable._
import scala.util.control.Breaks._
//https://stackoverflow.com/a/9727679/3740708
object Int {
    def unapply(v: String) = try Some(v.toInt) catch { case _: NumberFormatException => None }
}
object main {
    val TRACK_SIZE = 10
    val NUM_DICE = 3
    val DICE_SIZE = 100
    val WINNING_SCORE = 1000
    val WINNING_SCORE2 = 21

    def solvePart2(answers: Array[Array[Array[Array[Option[(Long, Long)]]]]], player1Pos: Int, player2Pos: Int, player1Score: Int, player2Score: Int): (Long, Long) = {
        if (player1Score >= WINNING_SCORE2) return (1, 0)
        if (player2Score >= WINNING_SCORE2) return (0, 1)
        answers(player1Pos)(player2Pos)(player1Score)(player2Score) match {
            case Some(scores) => scores
            case None => {
                var curPlayer1Wins = 0L
                var curPlayer2Wins = 0L
                for (dice1 <- Range(1, 4)) {
                    for (dice2 <- Range(1, 4)) {
                        for (dice3 <- Range(1, 4)) {
                            val newPlayer1Pos = (player1Pos+dice1+dice2+dice3) % TRACK_SIZE
                            val (tmp2, tmp1) = solvePart2(answers, player2Pos, newPlayer1Pos, player2Score, player1Score+newPlayer1Pos+1)
                            curPlayer1Wins += tmp1
                            curPlayer2Wins += tmp2
                        }
                    }
                }
                answers(player1Pos)(player2Pos)(player1Score)(player2Score) = Some((curPlayer1Wins, curPlayer2Wins))
                (curPlayer1Wins, curPlayer2Wins)
            }
        }
    }
    
    def main(args: Array[String]): Unit = {
        val puzzleInput = Source.fromFile(args(0)).getLines().toVector
        var s"Player 1 starting position: ${Int(player1Pos)}" = puzzleInput(0)
        var s"Player 2 starting position: ${Int(player2Pos)}" = puzzleInput(1)
        player1Pos -= 1
        player2Pos -= 1
        val origPlayer1Pos = player1Pos
        val origPlayer2Pos = player2Pos
        
        val commonDiff = NUM_DICE*NUM_DICE
        var curDiff = NUM_DICE*(NUM_DICE+1)/2
        var numTurns = 1
        var player1Score = 0
        var player2Score = 0
        breakable { while (true) {
            if (numTurns % 2 == 1) {
                player1Pos += curDiff
                player1Pos %= TRACK_SIZE
                player1Score += player1Pos+1
                if (player1Score >= WINNING_SCORE) break()
            } else {
                player2Pos += curDiff
                player2Pos %= TRACK_SIZE
                player2Score += player2Pos+1
                if (player2Score >= WINNING_SCORE) break()
            }
            curDiff += commonDiff
            numTurns += 1
        } }
        println(f"Part 1: ${Math.min(player1Score, player2Score)*NUM_DICE*numTurns}")

        var memo: Array[Array[Array[Array[Option[(Long, Long)]]]]] = Array.fill(TRACK_SIZE)(Array.fill(TRACK_SIZE)(Array.fill(WINNING_SCORE2)(Array.fill(WINNING_SCORE2)(None))))
        val (player1Wins, player2Wins) = solvePart2(memo, origPlayer1Pos, origPlayer2Pos, 0, 0)
        println(f"Part 2: ${Math.max(player1Wins, player2Wins)}")
    }
}