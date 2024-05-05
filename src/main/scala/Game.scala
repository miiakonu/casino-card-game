import scala.collection.mutable.Buffer

class Game:

  val players: Buffer[Player] = Buffer()
  
  private var roundNumber = 0
  
  var currentRound = Round(players, Deck(), roundNumber)
  
  def createRound() = // a method for creating a new round
    roundNumber += 1
    currentRound = Round(players, Deck(), roundNumber)
    currentRound.playerInTurn = players((roundNumber + 1) % players.size)
    players.foreach( _.playerDeck.clear() )
    players.foreach( _.playedCards.clear())
    players.foreach( _.moks = 0)

  def checkOver() = 
    isOver = !players.forall( _.points < 16)

  var isOver: Boolean = false

  def winner: String =
    if players.nonEmpty then
      if players.groupBy( _.points ).maxBy( _._1 )._2.size > 1 then
        players.groupBy( _.points ).maxBy( _._1 )._2.mkString(", ") // there can be multiple winners
      else
        players.maxBy( _.points ).name
    else
      "There's no players yet."


