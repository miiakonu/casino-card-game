import scala.collection.mutable.Buffer

class Player(val name: String):

  var playerHand: Buffer[Card] = Buffer()

  var playerDeck: Buffer[Card] = Buffer()

  var points: Int = 0

  val playedCards: Buffer[Card] = Buffer()

  var cardPlaySuccessful: Boolean = false

  var cardPickSuccessful: Boolean = false

  var moks = 0

  def playCardOntoTable(card: Card) =
    if playerHand.contains(card) then //checks if the player has the played card
      playerHand.remove(playerHand.indexOf(card))
      playedCards += card
      cardPlaySuccessful = true
    else
      cardPlaySuccessful = false
    if cardPlaySuccessful then
      println("Play successful!")

  def pickCardsFromTable(playedCard: Card, pickedCards: Buffer[Card]) = // method for picking up cards from the table with a certain card
    if pickedCards.size == pickedCards.toSet.size && playerHand.contains(playedCard) then // checks that there's no duplicates in the picked cards and that the player has the played card
      if pickedCards.size == 1 then
        if playedCard.value == pickedCards.head.number then
          playerDeck += playedCard
          playerDeck += pickedCards.head
          cardPickSuccessful = true
          // if the player only tries to pick one card, the method compares the picked and the played card, and if they match, they're added to the player's deck
        else
          cardPickSuccessful = false
      else
        val matchingCards: Buffer[Card] = Buffer()
        for card <- pickedCards do
          if playedCard.value == card.number then // compares every card one at a time to the played card
            matchingCards += card // and adds them to matchingCards if they match
          else
            for i <- 1 to (pickedCards.size) do
              if i == 1 then
                val nextCard = pickedCards((pickedCards.indexOf(card) + i) % pickedCards.size)
                if playedCard.value == card.number + nextCard.number then // compares the played card and two of the picked cards
                  matchingCards += card
                  matchingCards += nextCard
              else
                val testCards: Buffer[Card] = Buffer()
                for a <- 1 to i do
                    testCards += pickedCards((pickedCards.indexOf(card) + a) % pickedCards.size) // adds cards into testCards one at a time so every combination is tested
                val numbers = testCards.map(_.number)
                if playedCard.value == numbers.sum then
                  matchingCards ++= testCards // adds the cards into matchingCards if the sum is the same
        cardPickSuccessful = matchingCards.forall(card => pickedCards.contains(card)) && matchingCards.nonEmpty // if all cards in pickedCards have been added to matchingCards the pick is successful
    else
      cardPickSuccessful = false
    if cardPickSuccessful then 
      playerDeck ++= pickedCards
      playerDeck += playedCard
      playerDeck = playerDeck.distinct
      playerHand.remove(playerHand.indexOf(playedCard))
      println("Pick successful!")
    else
      println("Check that you can pick those cards.")

  override def toString = s"$name"

