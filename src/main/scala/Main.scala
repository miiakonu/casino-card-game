import scala.io.StdIn.readLine


@main
  def main() =
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

    var input = ""
    while input != "q!" do
      peli.checkOver()
      peli.currentRound.checkEnd()
      if peli.currentRound.hasEnded then 
        peli.currentRound.countPoints()
        println("The round has ended!")
        peli.players.foreach( player => println( player.name + " had " + player.moks + " moks."))
        println("Players' points: ")
        peli.players.foreach(player => println(player.name + " " + player.points))
        peli.createRound()
        peli.currentRound.dealCards()
        peli.currentRound.playerInTurn = peli.players(peli.players.indexOf(peli.currentRound.playerInTurn) + 1 % peli.players.size)
      else
        if peli.isOver then
          peli.currentRound.countPoints()
          println("The game is over!")
          println("The winner is: " + peli.winner)
          WriteFile(peli.players, peli).writeFile()
          input = "q!"
        else
          println("Now playing: " + peli.currentRound.playerInTurn.name )
          println("Your cards are: " + peli.currentRound.playerInTurn.playerHand.mkString(", "))
          println("Cards on table: " + peli.currentRound.cardsOnTable.mkString(", "))
          println(s"There are ${peli.currentRound.deck.cards.size} cards in the deck left." )
          input = readLine("> ")
          Action(input, peli).execute(peli.currentRound.playerInTurn)
          peli.currentRound.turnUpdate()
