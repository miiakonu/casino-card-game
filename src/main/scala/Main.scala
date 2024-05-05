import scala.io.StdIn.readLine

def printInstructions(): Unit = // welcome message
  println("When adding players separate the players' names with a comma")
  println("Play a card onto the table with the command: 'play'.\nPick cards from the table with the command: 'pick'.")
  println("When picking cards from the table, write first the card you're picking with,\nthen add a semicolon, and then the cards you want to pick separated with a comma.")
  println("Example: Pick heart 8; diamond 3, club 5 (when picking with heart 8).")
  println("When playing a card, just write the card after the command 'play'.")
  println("Example: Play club 10.")
  println("Don't write the numbers that are in brackets! The number tells you the card's value when it is in your hand.")
  println("Have fun playing :)\n")

@main
  def main() =
    printInstructions()
    val game = Game()
    println("Type q! to quit")

    println("Type all the players' names:\n(The maximum number of players is five.)")
    var names = readLine("> ").filter( _ != ' ')
    var buffer = names.split(',')
    def validPlayers = buffer.length <= 5 && buffer.length > 1
    while !validPlayers do
      println("The number of players has to be between 2 and 5.")
      names = readLine("> ").filter( _ != ' ')
      buffer = names.split(',')
    for name <- buffer do
      game.players += Player(name)
    println("Players have been added to the game!")

    game.createRound()
    game.currentRound.dealCards()

    var input = ""
    while input != "q!" do
      game.checkOver()
      game.currentRound.checkEnd()
      if game.currentRound.hasEnded then
        game.currentRound.countPoints()
        println("The round has ended!")
        game.players.foreach( player => println( player.name + " had " + player.moks + " moks."))
        game.players.foreach( player => println( player.name + "'s deck: " + player.playerDeck.mkString(", ")))
        println("Players' points: ")
        game.players.foreach(player => println(player.name + " " + player.points))
        game.createRound()
        game.currentRound.dealCards()
      else
        if game.isOver then
          game.currentRound.countPoints()
          println("The game is over!")
          println("The winner is: " + game.winner)
          WriteFile(game.players, game).writeFile()
          input = "q!"
        else
          println("Now playing: " + game.currentRound.playerInTurn.name )
          println("Your cards are: " + game.currentRound.playerInTurn.playerHand.mkString(", "))
          println("Cards on table: " + game.currentRound.cardsOnTable.mkString(", "))
          println(s"There are ${game.currentRound.deck.cards.size} cards in the deck left." )
          input = readLine("> ")
          Action(input, game).execute(game.currentRound.playerInTurn)
          game.currentRound.turnUpdate()