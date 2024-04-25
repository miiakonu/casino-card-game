import scala.collection.mutable.Buffer

class Action(input: String, game: Game):

    val commandText = input.trim.toLowerCase.filter( _ != ' ')
    val verb        = commandText.take(4)

    val kortti   = commandText.drop(verb.length).takeWhile( _.isLetter )
    val numero = commandText.drop(verb.length + kortti.length).takeWhile( _ != ';' )

    val kortit = commandText.dropWhile( _ != ';').filter( _ != ';')

    def findInHand(cardsInHand: Buffer[Card], suit: String, number: Int) =
      if !game.currentRound.deck.suits.contains(suit)  then
        throw InvalidDataException("Invalid command: try again.")
      if !1.to(13).contains(number) then
        throw InvalidDataException("Invalid command: try again.")
      var kortti = cardsInHand.head
      for card <- cardsInHand do
        if card.suit == suit && card.number == number then
          kortti = card
      kortti
          // muuta niin että tarkistaa että onhan kortti kädessä

      // if !cardsInHand.forall(card => card.suit == suit && card.number == number) then

    val stringsToCards: Map[String, Card] =
      var map = Map[String, Card]()
      for card <- game.currentRound.deck.cards do
        map += (s"${card.suit} ${card.number}" -> card)
      map

    def toBuffer(cards: String) =
      val buffer = cards.split(',')
      val cardBuffer: Buffer[Card] = Buffer()
      for card <- buffer do
        cardBuffer += Card(card.takeWhile( _.isLetter ), card.dropWhile( _.isLetter ).toInt)
      cardBuffer

    def findInTable(cardsOnTable: Buffer[Card], otherCards: Buffer[Card]) =
      val bufferCards: Buffer[Card] = Buffer()
      for card <- otherCards do
        if !game.currentRound.deck.suits.contains(card.suit) then
          throw InvalidDataException("Invalid command: try again.")
        if !1.to(13).contains(card.number) then
          throw InvalidDataException("Invalid command: try again.")
        for card2 <- cardsOnTable do
          if card.suit == card2.suit && card.number == card2.number then
            bufferCards += card2
      bufferCards

    def execute(actor: Player) =
      this.verb match
        case "play" =>
          if numero.length > 2 then throw InvalidDataException("Invalid command: try again.")
            if actor.playerHand.exists(card => card.suit == kortti && card.number == numero.toInt) then
              actor.playCardOntoTable(findInHand(actor.playerHand, kortti, numero.toInt))
            else println("Play failed; check that you typed the card correctly.")
        case "pick" =>
          try
            if !numero.forall(_.isDigit) then throw InvalidDataException("Invalid command: try again.")
            if actor.playerHand.exists(card => card.suit == kortti && card.number == numero.toInt) then
              actor.pickCardsFromTable(findInHand(actor.playerHand, kortti, numero.toInt), findInTable(game.currentRound.cardsOnTable, toBuffer(kortit)))
            else println("Pick failed; check that you typed the card correctly.")
          catch
            case e: InvalidDataException => println(e.text)
        case _ => println("Use the instructed commands.")
