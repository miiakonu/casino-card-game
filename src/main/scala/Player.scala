import scala.collection.mutable.Buffer

class Player(val name: String):

  var playerHand: Buffer[Card] = Buffer()

  val playerDeck: Buffer[Card] = Buffer()

  var points: Int = 0

  val playedCards: Buffer[Card] = Buffer()

  var cardPlaySuccessful: Boolean = false

  var cardPickSuccessful: Boolean = false

  var moks = 0

  def playCardOntoTable(card: Card) =
    if playerHand.contains(card) then
      playerHand.remove(playerHand.indexOf(card))
      playedCards += card
      cardPlaySuccessful = true
    else
      cardPlaySuccessful = false
    if cardPlaySuccessful then
      println("Play successful!")

  def pickCardsFromTable(playedCard: Card, pickedCards: Buffer[Card]) = // method for picking up cards from the table with a certain card
    if playerHand.contains(playedCard) then
      if pickedCards.size == 1 then
        if playedCard.value == pickedCards.head.number then
          playerDeck += playedCard
          playerDeck += pickedCards.head
          cardPickSuccessful = true
        else
          cardPickSuccessful = false
      else
        val matchingCards: Buffer[Card] = Buffer()
        for card <- pickedCards do
          if playedCard.value == card.number then
            matchingCards += card
          else
            for i <- 1 until (pickedCards.size) do
              if i == 1 then
                val nextCard = pickedCards((pickedCards.indexOf(card) + i) % pickedCards.size)
                if playedCard.value == card.number + nextCard.number then
                  matchingCards += card
                  matchingCards += nextCard
              else
                val testCards: Buffer[Card] = Buffer()
                for a <- 1 to i do
                    testCards += pickedCards((pickedCards.indexOf(card) + a) % pickedCards.size)
                val numbers = testCards.map(_.number)
                if playedCard.value == numbers.sum then
                  matchingCards ++= testCards
        cardPickSuccessful = matchingCards.forall(card => pickedCards.contains(card))
    else
      cardPickSuccessful = false
    if cardPickSuccessful then
      playerDeck ++= pickedCards
      playerDeck += playedCard
      playerHand.remove(playerHand.indexOf(playedCard))
      println("Pick successful!")
    else
      println("Check that you can pick those cards.")

      // muista lisätä että samaa korttia ei voi käyttää kahdesti




  override def toString = s"$name"