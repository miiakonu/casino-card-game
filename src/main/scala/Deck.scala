import scala.collection.mutable.Buffer
import scala.util.Random

class Deck:

  val suits = Vector("spade", "heart", "diamond", "club")

  val cards: Buffer[Card] =
    val bufferCards: Buffer[Card] = Buffer()
    for i <- 1 to 13 do
      for suit <- suits do
        bufferCards += Card(suit, i)
    Random.shuffle(bufferCards)

