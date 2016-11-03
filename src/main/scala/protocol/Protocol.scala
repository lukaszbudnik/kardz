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

// in case there is a draw Referee should ask for one hidden and one normal card
case object AskForHiddenCard

case class GiveHiddenCard(card: Card)
