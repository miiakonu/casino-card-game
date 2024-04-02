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
  round.cardsOnTable = Buffer(Card("Diamond", 2), Card("Heart", 9), Card("Heart", 5), Card("Club", 3))
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

class Action(input: String):

    val commandText = input.trim.toLowerCase
    val verb        = commandText.takeWhile( _ != ' ' )
    val kortti   = commandText.drop(verb.length + 1).takeWhile( _ != ' ' )
    val numero = commandText.drop(verb.length + kortti.length + 2).trim.toInt

    def execute(actor: Player) = this.verb match
      case "play" => actor.playCardOntoTable(Card(kortti, numero))

@main
  def other() =
    val peli = Game()
    val round = peli.currentRound
    round.dealCards()
    println("Type q! to quit")

    println("Your cards are: " + round.playerInTurn.playerHand)
    println("Cards on table: " + round.cardsOnTable)

    var input = readLine("> ")

    while input != "q!" do
      Action(input).execute(round.playerInTurn)
      round.turnUpdate()
      println("Your cards are: " + round.playerInTurn.playerHand)
      println("Cards on table: " + round.cardsOnTable)
      input = readLine("> ")







