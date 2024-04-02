class Card(val suit: String, val number: Int):
  
  val value = number
  
  override def toString = s"$suit $number"

