import scala.collection.mutable.Buffer
import scala.util.Random

class Deck:
  
  val cards: Buffer[Card] =
    val suits = Vector("Spade", "Heart", "Diamond", "Club")
    val bufferCards: Buffer[Card] = Buffer()
    for i <- 2 to 13 do
      for suit <- suits do
        bufferCards += Card(suit, i)
    Random.shuffle(bufferCards)

