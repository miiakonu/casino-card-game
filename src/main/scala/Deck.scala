import scala.collection.mutable.Buffer
import scala.util.Random

class Deck:

  val cards: Buffer[Card] =
    val suits = Vector("spade", "heart", "diamond", "club")
    val bufferCards: Buffer[Card] = Buffer()
    for i <- 1 to 4 do
      for suit <- suits do
        bufferCards += Card(suit, i)
    Random.shuffle(bufferCards)

