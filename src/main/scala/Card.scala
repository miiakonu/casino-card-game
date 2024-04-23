class Card(val suit: String, val number: Int):
  
  val value =
    if number == 10 && suit == "diamond" then 16
    else if number == 1 then 14
    else if number == 2 && suit == "spade" then 15
    else number
  
  override def toString =
    if number != value then s"$suit $number ($value)"
    else s"$suit $number"

