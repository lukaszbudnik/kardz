package protocol

case object Distribute

case object Start

case class Card(rank: Rank, colour: Colour)

case object AskForCard

case class GiveCard(card: Card)

case object NoCard

case object Check

case object CountCards

case class NumberOfCards(n: Int)

case class Winner(player: Int)
