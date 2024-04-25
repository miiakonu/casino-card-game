import scala.collection.mutable.Buffer
import scala.util.Random

class Game:

  val players: Buffer[Player] = Buffer()
  var roundNumber = 0
  var currentRound = Round(players, Deck(), roundNumber)
  def createRound() =
    roundNumber += 1
    currentRound = Round(players, Deck(), roundNumber)
    currentRound.dealer = players((roundNumber - 1) % players.size)

  def checkOver() = 
    isOver = !players.forall( _.points < 16)

  var isOver: Boolean = false
