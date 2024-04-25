import scalafx.application.JFXApp3
import scalafx.scene.Scene
import scalafx.scene.layout.Pane
import scalafx.scene.shape.Rectangle
import scalafx.scene.paint.Color._
import scala.collection.mutable.Buffer

import scala.io.StdIn.readLine

/*
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
  println("whose turn it is: " + round.playerInTurn) */


@main
  def other() =
    val peli = Game()
    println("Type q! to quit")

    println("Type all the players' names:\n(The maximum number of players is five.)")
    var names = readLine("> ").filter( _ != ' ')
    var buffer = names.split(',')
    def validPlayers = buffer.length <= 5 && buffer.length > 1
    while !validPlayers do
      println("The maximum number of players is five.")
      names = readLine("> ").filter( _ != ' ')
      buffer = names.split(',')
    for name <- buffer do
      peli.players += Player(name)
    println("Players have been added to the game!")

    peli.createRound()
    peli.currentRound.dealCards()

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
          println(peli.currentRound.playerInTurn.playerDeck) //testi
