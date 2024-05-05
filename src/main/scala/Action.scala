import scala.collection.mutable.Buffer

class Action(input: String, game: Game):

    private val commandText = input.trim.toLowerCase.filter( _ != ' ')
    private val verb        = commandText.take(4)

    private val inputCard   = commandText.drop(verb.length).takeWhile( _.isLetter )
    private val inputNumber = commandText.drop(verb.length + inputCard.length).takeWhile( _ != ';' )

    private val inputCards = commandText.dropWhile( _ != ';').filter( _ != ';')

    private def findInHand(cardsInHand: Buffer[Card], suit: String, number: Int) =
      if !game.currentRound.deck.suits.contains(suit)  then
        throw InvalidDataException("Invalid command: try again.")
      if !1.to(13).contains(number) then
        throw InvalidDataException("Invalid command: try again.")
      var kortti = cardsInHand.head
      for card <- cardsInHand do
        if card.suit == suit && card.number == number then
          kortti = card
      kortti // returns the card from the collection (cardsInHand) that has the specific suit and number

    private def toBuffer(cards: String) =
      val buffer = cards.split(',')
      val cardBuffer: Buffer[Card] = Buffer()
      try
        for card <- buffer do
          if !card.dropWhile( _.isLetter ).forall(_.isDigit) || card.dropWhile( _.isLetter ).isBlank then
            throw InvalidDataException("Invalid command: try again.")
          else
            cardBuffer += Card(card.takeWhile( _.isLetter ), card.dropWhile( _.isLetter ).toInt)
      catch
        case e: InvalidDataException => println(e.text)
      cardBuffer // turns one string into a buffer with cards

    private def findInTable(cardsOnTable: Buffer[Card], otherCards: Buffer[Card]) =
      val bufferCards: Buffer[Card] = Buffer()
      for card <- otherCards do
        if !game.currentRound.deck.suits.contains(card.suit) then
          throw InvalidDataException("Invalid command: try again.")
        if !1.to(13).contains(card.number) then
          throw InvalidDataException("Invalid command: try again.")
        for card2 <- cardsOnTable do
          if card.suit == card2.suit && card.number == card2.number then
            bufferCards += card2
      bufferCards // returns the cards that are in both collections

    def execute(actor: Player) =
      this.verb match
        case "play" =>
          try
            if inputNumber.length > 2 || !inputNumber.forall( _.isDigit) || inputNumber.isBlank then // checks if the input is valid
              throw InvalidDataException("Invalid command: try again.")
            else
              if actor.playerHand.exists(card => card.suit == inputCard && card.number == inputNumber.toInt) then // checks that the player has that card
                actor.playCardOntoTable(findInHand(actor.playerHand, inputCard, inputNumber.toInt))
              else println("Play failed; check that you typed the card correctly.")
          catch
            case e: InvalidDataException => println(e.text)
        case "pick" =>
          try
            if inputNumber.length > 2 || !inputNumber.forall( _.isDigit) || inputNumber.isBlank then // checks if the input is valid
              throw InvalidDataException("Invalid command: try again.")
            else
              if actor.playerHand.exists(card => card.suit == inputCard && card.number == inputNumber.toInt) then // checks that the player has that card
                actor.pickCardsFromTable(findInHand(actor.playerHand, inputCard, inputNumber.toInt), findInTable(game.currentRound.cardsOnTable, toBuffer(inputCards)))
              else println("Pick failed; check that you typed the card correctly.")
          catch
            case e: InvalidDataException => println(e.text)
        case _ => println("Use the instructed commands.")
