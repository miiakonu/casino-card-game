import scala.collection.mutable.Buffer

class Player(val name: String):
  var playerHand: Buffer[Card] = Buffer()