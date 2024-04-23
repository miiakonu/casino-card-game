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

  var playerWhoLastPicked: Player = dealer

  def turnUpdate() = // updates the game situation after a turn

    if playerInTurn.cardPlaySuccessful then // if the player plays a card to the table
      cardsOnTable += playerInTurn.playedCards.last // the player's last played card is added onto the table
      if deck.cards.nonEmpty then
        playerInTurn.playerHand += deck.cards.head // and a new card is added to the players hand from the deck
        deck.cards.remove(0)
      playerInTurn.cardPlaySuccessful = false
      playerInTurn = players(if players.indexOf(playerInTurn) + 1 <= players.size - 1 then players.indexOf(playerInTurn) + 1
        else (players.indexOf(playerInTurn) + 1) % players.size) // the turn moves to the next player

    if playerInTurn.cardPickSuccessful then
      for card <- playerInTurn.playerDeck do
        if cardsOnTable.contains(card) then cardsOnTable.remove(cardsOnTable.indexOf(card))
      if deck.cards.nonEmpty then
        playerInTurn.playerHand += deck.cards.head
        deck.cards.remove(0)
      playerInTurn.cardPickSuccessful = false
      playerWhoLastPicked = playerInTurn
      playerInTurn = players(if players.indexOf(playerInTurn) + 1 <= players.size - 1 then players.indexOf(playerInTurn) + 1
        else (players.indexOf(playerInTurn) + 1) % players.size)


  def checkEnd() =
    if players.forall(_.playerHand.isEmpty) then
      hasEnded = true
    else hasEnded = false

  var hasEnded: Boolean = false

  def countPoints() =
    playerWhoLastPicked.playerDeck ++= cardsOnTable // player who last picked gets all the cards on the table onto their deck
    for player <- players do
      player.points += player.moks
      player.points += player.playerDeck.count( _.number == 1 )
      if player.playerDeck.exists( _.value == 16) then player.points += 2
      if player.playerDeck.exists( _.value == 15) then player.points += 1
    val playerWithMostSpades = players.groupBy( _.playerDeck.count(_.suit == "spade" )).maxBy(_._1)._2
    if playerWithMostSpades.size == 1 then playerWithMostSpades.head.points += 2 // jos usealla pelaajalla on suurin määrä patoja, kumpikaan ei saa niistä pisteitä
    val playerWithMostCards = players.groupBy( _.playerDeck.size).maxBy(_._1)._2
    if playerWithMostCards.size == 1 then playerWithMostCards.head.points += 1 // jos usealla pelaajalla on suurin määrä kortteja, kumpikaan ei saa niistä pistettä





