import scala.collection.mutable.Buffer

class Game:

  val player1 = Player("Miia")
  val player2 = Player("Oona")
  val players: Buffer[Player] = Buffer(player1, player2)
  val deck = Deck()
  val currentRound = Round(players, players.head, deck, 1)

