import java.io.*
import scala.collection.mutable.Buffer


class WriteFile(players: Buffer[Player], game: Game):

  def writeFile() =
    try
      val file = File("src/main/scala/gamedata.txt")
      val time = "date: " + java.time.LocalDate.now.toString + "\n"
      val names = "players: " + players.map( _.name ).mkString(", ") + "\n"
      val info = players.map( player => player.name + "'s points: " + player.points.toString).mkString("\n")
      val bw = BufferedWriter(FileWriter(file))
      bw.write(time)
      bw.write(names)
      bw.write(info)
      bw.write("\nThe winner: " + game.winner)
      bw.close()
    catch
      case e: IOException => println("Error with writing the file.")



