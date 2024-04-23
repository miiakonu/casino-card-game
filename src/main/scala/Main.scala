import scalafx.application.JFXApp3
import scalafx.scene.Scene
import scalafx.scene.layout.Pane
import scalafx.scene.shape.Rectangle
import scalafx.scene.paint.Color._
import scala.collection.mutable.Buffer

import scala.io.StdIn.readLine

object Main extends JFXApp3:

  def start() =

    stage = new JFXApp3.PrimaryStage:
      title = "UniqueProjectName"
      width = 600
      height = 450

    val root = Pane()

    val scene = Scene(parent = root)
    stage.scene = scene

    val rectangle = new Rectangle:
      x = 275
      y = 175
      width = 50
      height = 50
      fill = Blue

    root.children += rectangle

  end start

end Main

@main
  def main() =
  val peli = Game()
  val round = peli.currentRound
  peli.player1.playerHand = Buffer(Card("Spade", 2), Card("Heart", 3), Card("Spade", 4), Card("Club", 5))
  peli.player2.playerHand = Buffer(Card("Spade", 5), Card("Heart", 10), Card("Spade", 6), Card("Club", 2))
  round.cardsOnTable = Buffer(Card("Diamond", 3), Card("Heart", 9), Card("Heart", 4), Card("Club", 3))
  val card = peli.player2.playerHand.head
  println("players: " + peli.players)
  println("players' hands: ")
  peli.players.foreach(player => println(player.name + " " + player.playerHand))
  println("cards on table: " + round.cardsOnTable)

  println("\nnew turn")
  println("whose turn it is: " + round.playerInTurn)
  println("the player's hand: " + round.playerInTurn.playerHand)
  println("plays the card: " + round.playerInTurn.playerHand.head)
  round.playerInTurn.playCardOntoTable(round.playerInTurn.playerHand.head)
  round.turnUpdate()
  println("cards on table: " + round.cardsOnTable)

  println("\nnew turn")
  println("whose turn it is: " + round.playerInTurn)
  println("the player's hand: " + round.playerInTurn.playerHand)
  println("plays the card: " + card)
  round.playerInTurn.pickCardsFromTable(card, Buffer(round.cardsOnTable.head, round.cardsOnTable(3), round.cardsOnTable(2)))
  round.turnUpdate()
  peli.players.foreach(player => println(player.name + " " + player.playerHand))
  println("cards on table: " + round.cardsOnTable)
  println(peli.player2.playerDeck)

  println("\nnew turn")
  println("whose turn it is: " + round.playerInTurn)

class Action(input: String, game: Game):

    val commandText = input.trim.toLowerCase
    val verb        = commandText.takeWhile( _ != ' ' )

    val kortti   = commandText.drop(verb.length + 1).takeWhile( _ != ' ' )
    val numero = commandText.drop(verb.length + kortti.length + 2).takeWhile( _ != ';' ).trim
    val nostonumero = commandText.drop(verb.length + kortti.length + 2).takeWhile( _ != ';' ) // tarkista voiko poistaa

    val kortit = commandText.dropWhile( _ != ';').filter( char => char != ' ' && char != ';')

    def findInHand(cardsInHand: Buffer[Card], suit: String, number: Int) =
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
        for card2 <- cardsOnTable do
          if card.suit == card2.suit && card.number == card2.number then
            bufferCards += card2
      bufferCards

    def execute(actor: Player) =
      this.verb match
        case "play" =>
          if actor.playerHand.exists(card => card.suit == kortti && card.number == numero.toInt) then
            actor.playCardOntoTable(findInHand(actor.playerHand, kortti, numero.toInt))
          else println("Play failed; check that you typed the card correctly.")
        case "pickwith" =>
          if actor.playerHand.exists(card => card.suit == kortti && card.number == numero.toInt) then
            actor.pickCardsFromTable(findInHand(actor.playerHand, kortti, numero.toInt), findInTable(game.currentRound.cardsOnTable, toBuffer(kortit)))
          else println("Pick failed; check that you typed the card correctly.")

@main
  def other() =
    val peli = Game()
    peli.currentRound.dealCards()
    println("Type q! to quit")

    println("Now playing: " + peli.currentRound.playerInTurn.name )
    println("Your cards are: " + peli.currentRound.playerInTurn.playerHand.mkString(", "))
    println("Cards on table: " + peli.currentRound.cardsOnTable.mkString(", "))

    var input = ""
    while input != "q!" do
      peli.checkOver()
      peli.currentRound.checkEnd()
      if peli.currentRound.hasEnded then 
        peli.currentRound.countPoints()
        peli.createRound()
        peli.currentRound.dealCards()
        println("The round has ended!")
        println("Players' points: ")
        peli.players.foreach(player => println(player.name + " " + player.points))

      else
        if peli.isOver then
          peli.currentRound.countPoints()
          println("The game is over!")
          println("The winner is: " + peli.players.maxBy( _.points ))
        else
          input = readLine("> ")
          Action(input, peli).execute(peli.currentRound.playerInTurn)
          peli.currentRound.turnUpdate()
          println("Now playing: " + peli.currentRound.playerInTurn.name )
          println("Your cards are: " + peli.currentRound.playerInTurn.playerHand.mkString(", "))
          println("Cards on table: " + peli.currentRound.cardsOnTable.mkString(", "))

