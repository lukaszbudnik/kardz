package engine

import java.util
import java.util.Collections

import protocol.{Colour, Rank, Card}

import scala.collection.JavaConversions._

object GameEngine {

  def distribute: Seq[Card] = {

    val cards = new util.ArrayList[Card]()
    for {
      r <- Rank.values()
      c <- Colour.values()
    } {
      cards.add(Card(r, c))
    }

    Collections.shuffle(cards)

    cards
  }

  def winnerByCards(cards: Seq[Card]): Seq[Int] = {
    val max = cards.maxBy(c => c.rank.ordinal())
    val maxCards = cards.filter(c => c.rank == max.rank)
    maxCards.map(c => cards.indexOf(c))
  }

  def winnerByNumberOfCards(numberOfCards: Seq[Int]): Int = {
    val max = numberOfCards.max
    numberOfCards.indexOf(max)
  }

}
