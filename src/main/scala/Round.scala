import scala.collection.mutable.Buffer

class Round(val game: Game, val dealer: Player, val deck: Deck):
  val cardsOnTable: Buffer[Card] = Buffer()
  def dealCards(): Unit =
    for player <- game.players do
      player.playerHand = deck.cards.take(4)
      deck.cards.remove(0, 4)


