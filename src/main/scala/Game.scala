import scala.collection.mutable.Buffer
import scala.util.Random

class Game:

  val player1 = Player("Miia")
  val player2 = Player("Oona")
  val players: Buffer[Player] = Buffer(player1, player2)
  var roundNumber = 1
  var currentRound = Round(players, players.head, Deck(), roundNumber)
  def createRound() =
    roundNumber += 1
    currentRound = Round(players, players((roundNumber - 1) % players.size), Deck(), roundNumber)

  def checkOver() = 
    isOver = !players.forall( _.points < 16)

  var isOver: Boolean = false
