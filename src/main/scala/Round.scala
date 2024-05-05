import scala.collection.mutable.Buffer

class Round(players: Buffer[Player], val deck: Deck, numberOfRound: Int):

  var cardsOnTable: Buffer[Card] =
    val cards = deck.cards.take(4)
    deck.cards.remove(0, 4)
    cards

  def dealCards(): Unit =
    for player <- players do
      player.playerHand = deck.cards.take(4)
      deck.cards.remove(0, 4)

  var playerInTurn: Player = if players.nonEmpty then players.head else Player("Miia :D")

  private var playerWhoLastPicked: Option[Player] = None

  def turnUpdate() = // updates the game situation after a turn

    if playerInTurn.cardPlaySuccessful then // if the player plays a card to the table
      cardsOnTable += playerInTurn.playedCards.last // the player's last played card is added onto the table
      if deck.cards.nonEmpty then
        playerInTurn.playerHand += deck.cards.head // and a new card is added to the players hand from the deck
        deck.cards.remove(0)
      playerInTurn.cardPlaySuccessful = false
      playerInTurn = players(if players.indexOf(playerInTurn) + 1 <= players.size - 1 then players.indexOf(playerInTurn) + 1
        else (players.indexOf(playerInTurn) + 1) % players.size) // the turn moves to the next player

    if playerInTurn.cardPickSuccessful then // if the player picks cards from the table
      for card <- playerInTurn.playerDeck do
        if cardsOnTable.contains(card) then cardsOnTable.remove(cardsOnTable.indexOf(card)) // those cards are removed from table
      if deck.cards.nonEmpty then 
        playerInTurn.playerHand += deck.cards.head // a new card is added to the players hand from the deck
        deck.cards.remove(0) 
      if cardsOnTable.isEmpty then playerInTurn.moks += 1 // if the player empties the table, they get one "mökki"
      playerInTurn.cardPickSuccessful = false
      playerWhoLastPicked = Some(playerInTurn) // this is saved so the last player who picks gets all the cards on the table
      playerInTurn = players(if players.indexOf(playerInTurn) + 1 <= players.size - 1 then players.indexOf(playerInTurn) + 1
        else (players.indexOf(playerInTurn) + 1) % players.size) // and the turn moves to the next player

  def checkEnd() =
    if players.forall(_.playerHand.isEmpty) then
      hasEnded = true
    else hasEnded = false

  var hasEnded: Boolean = false

  def countPoints() =
    playerWhoLastPicked match
      case Some(player) => player.playerDeck ++= cardsOnTable // player who last picked gets all the cards on the table onto their deck
      case None =>
    for player <- players do
      player.points += player.moks
      player.points += player.playerDeck.count( _.number == 1 )
      if player.playerDeck.exists( _.value == 16) then player.points += 2
      if player.playerDeck.exists( _.value == 15) then player.points += 1
    val playerWithMostSpades = players.groupBy( _.playerDeck.count(_.suit == "spade" )).maxBy(_._1)._2
    if playerWithMostSpades.size == 1 then playerWithMostSpades.head.points += 2 // if many players have most spades, no one gets the points
    val playerWithMostCards = players.groupBy( _.playerDeck.size).maxBy(_._1)._2
    if playerWithMostCards.size == 1 then playerWithMostCards.head.points += 1 // if many players have most cards, no one gets the points
