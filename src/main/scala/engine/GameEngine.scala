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

  def winnerByCards(cards: Seq[Seq[Card]]): Seq[Int] = {
    val longestSize = cards.map(_.size).max
    val longest = cards.filter(_.size == longestSize)
    // last man standing
    if (longest.size == 1) {
      Seq(cards.indexOf(longest.head))
    } else {

      val lastCards = cards.map { c =>
        if (c.size == longestSize) {
          Some(c.last)
        } else {
          None
        }
      }
          val max = lastCards.maxBy(_ match {
            case Some(c) => c.rank.ordinal()
            case None => -1
          }).map(_.rank)

          val maxCards = lastCards.filter(_ match {
            case Some(c) => Some(c.rank) == max
            case None => false
          })

      val winners = maxCards.map { c =>
        lastCards.indexOf(c)
      }

      winners
    }
  }

  def winnerByNumberOfCards(numberOfCards: Seq[Int]): Int = {
    val max = numberOfCards.max
    numberOfCards.indexOf(max)
  }

}
