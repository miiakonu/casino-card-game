import scala.collection.mutable.Buffer

class Player(val name: String):

  var playerHand: Buffer[Card] = Buffer()

  val playerDeck: Buffer[Card] = Buffer()

  var points: Int = 0

  val playedCards: Buffer[Card] = Buffer()

  var cardPlaySuccessful: Boolean = false

  var cardPickSuccessful: Boolean = false

  def playCardOntoTable(card: Card) =
    if playerHand.contains(card) then
      playerHand.remove(playerHand.indexOf(card))
      playedCards += card
      cardPlaySuccessful = true
    else
      cardPlaySuccessful = false

  def pickCardsFromTable(playedCard: Card, pickedCards: Buffer[Card]) = // method for picking up cards from the table with a certain card
    if playerHand.contains(playedCard) then
      if pickedCards.size == 1 then
        if playedCard.value == pickedCards.head.value then
          playerDeck += playedCard
          playerDeck += pickedCards.head
          cardPickSuccessful = true
        else
          cardPickSuccessful = false
      else
        val matchingCards: Buffer[Card] = Buffer()
        for card <- pickedCards do
          if playedCard.value == card.value then
            matchingCards += card
          else
            for i <- 1 until (pickedCards.size) do
              if i == 1 then
                val nextCard = pickedCards((pickedCards.indexOf(card) + i) % pickedCards.size)
                if playedCard.value == card.value + nextCard.value then
                  matchingCards += card
                  matchingCards += nextCard
              else
                val testCards: Buffer[Card] = Buffer()
                for a <- 1 to i do
                    testCards += pickedCards((pickedCards.indexOf(card) + a) % pickedCards.size)
                val values = testCards.map(_.value)
                if playedCard.value == values.sum then
                  matchingCards ++ testCards
        cardPickSuccessful = matchingCards.forall(card => pickedCards.contains(card))
    else
      cardPickSuccessful = false
    if cardPickSuccessful then
      playerDeck ++= pickedCards
      playerDeck += playedCard
      playerHand.remove(playerHand.indexOf(playedCard))
      println("success")
    else
      println("not this time")
      
      // muista lisätä että samaa korttia ei voi käyttää kahdesti


  override def toString = s"$name"