import scala.collection.mutable.Buffer

class Round(val players: Buffer[Player], val dealer: Player, val deck: Deck, numberOfRound: Int):

  var cardsOnTable: Buffer[Card] =
    val cards = deck.cards.take(4)
    deck.cards.remove(0, 4)
    cards


  def dealCards(): Unit =
    for player <- players do
      player.playerHand = deck.cards.take(4)
      deck.cards.remove(0, 4)

  var playerInTurn: Player = dealer

  def turnUpdate() = // updates the game situation after a turn

    if playerInTurn.cardPlaySuccessful then // if the player plays a card to the table
      cardsOnTable += playerInTurn.playedCards.last // the player's last played card is added onto the table
      playerInTurn.playerHand += deck.cards.head // and a new card is added to the players hand from the deck
      deck.cards.remove(0)
      playerInTurn = players(if players.indexOf(playerInTurn) + 1 <= players.size - 1 then players.indexOf(playerInTurn) + 1
        else (players.indexOf(playerInTurn) + 1) % players.size) // the turn moves to the next player

    if playerInTurn.cardPickSuccessful then
      for card <- playerInTurn.playerDeck do
        if cardsOnTable.contains(card) then cardsOnTable.remove(cardsOnTable.indexOf(card))
      playerInTurn.playerHand += deck.cards.head
      deck.cards.remove(0)
      playerInTurn = players(if players.indexOf(playerInTurn) + 1 <= players.size - 1 then players.indexOf(playerInTurn) + 1
        else (players.indexOf(playerInTurn) + 1) % players.size)


  var hasEnded: Boolean = false


