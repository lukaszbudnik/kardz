package engine

import org.junit.runner.RunWith
import org.specs2.mutable.Specification
import org.specs2.runner.JUnitRunner
import protocol.{Colour, Rank, Card}


@RunWith(classOf[JUnitRunner])
class GameEngineSpec extends Specification {

  "The GameEngine" should {

    "return 52 random cards" in {
      val cards = GameEngine.distribute
      cards must haveSize(52)
      }

    "return winner for list of cards" in {
      val card1 = Card(Rank.EIGHT, Colour.CLUBS)
      val card2 = Card(Rank.QUEEN, Colour.HEARTS)
      val card3 = Card(Rank.ACE, Colour.SPADES)
      val card4 = Card(Rank.TWO, Colour.DIAMONDS)
      val cards = Seq(Seq(card1), Seq(card2), Seq(card3), Seq(card4))
      val winner = GameEngine.winnerByCards(cards)
      winner must beEqualTo(Seq(2))
    }

    "return winners for list of same cards" in {
      val card1 = Card(Rank.ACE, Colour.CLUBS)
      val card2 = Card(Rank.QUEEN, Colour.HEARTS)
      val card3 = Card(Rank.ACE, Colour.SPADES)
      val card4 = Card(Rank.ACE, Colour.DIAMONDS)
      val cards = Seq(Seq(card1), Seq(card2), Seq(card3), Seq(card4))
      val winner = GameEngine.winnerByCards(cards)
      winner must beEqualTo(Seq(0,2,3))
    }

    "return winner for list of cards in war" in {
      val card1 = Card(Rank.ACE, Colour.CLUBS)
      val card2 = Card(Rank.QUEEN, Colour.HEARTS)
      val card3 = Card(Rank.ACE, Colour.SPADES)
      val card4 = Card(Rank.ACE, Colour.DIAMONDS)

      val card1b = Card(Rank.THREE, Colour.DIAMONDS)
      val card3b = Card(Rank.TWO, Colour.CLUBS)
      val card4b = Card(Rank.JACK, Colour.DIAMONDS)

      val cards = Seq(Seq(card1, card1b), Seq(card2), Seq(card3, card3b), Seq(card4, card4b))
      val winner = GameEngine.winnerByCards(cards)
      winner must beEqualTo(Seq(3))
    }

    "return winners for list of same cards in war" in {
      val card1 = Card(Rank.ACE, Colour.CLUBS)
      val card2 = Card(Rank.QUEEN, Colour.HEARTS)
      val card3 = Card(Rank.ACE, Colour.SPADES)
      val card4 = Card(Rank.ACE, Colour.DIAMONDS)

      val card1b = Card(Rank.THREE, Colour.DIAMONDS)
      val card3b = Card(Rank.JACK, Colour.CLUBS)
      val card4b = Card(Rank.JACK, Colour.DIAMONDS)

      val cards = Seq(Seq(card1, card1b), Seq(card2), Seq(card3, card3b), Seq(card4, card4b))
      val winner = GameEngine.winnerByCards(cards)
      winner must beEqualTo(Seq(2,3))
    }

    "return winner for the largest list of cards in war" in {
      val card1 = Card(Rank.ACE, Colour.CLUBS)
      val card2 = Card(Rank.QUEEN, Colour.HEARTS)
      val card3 = Card(Rank.ACE, Colour.SPADES)
      val card4 = Card(Rank.ACE, Colour.DIAMONDS)

      val card1b = Card(Rank.THREE, Colour.DIAMONDS)
      val card3b = Card(Rank.JACK, Colour.CLUBS)
      val card4b = Card(Rank.JACK, Colour.DIAMONDS)

      val card3c = Card(Rank.FOUR, Colour.CLUBS)

      val cards = Seq(Seq(card1, card1b), Seq(card2), Seq(card3, card3b, card3c), Seq(card4, card4b))
      val winner = GameEngine.winnerByCards(cards)
      winner must beEqualTo(Seq(2))
    }

    "return one winner for list of number of cards" in {
      val cards = Seq(12, 23, 3, 6)
      val winners = GameEngine.winnerByNumberOfCards(cards)
      winners must beEqualTo(Seq(1))
    }

    "return many winners for list of number of cards" in {
      val cards = Seq(12, 23, 3, 23)
      val winners = GameEngine.winnerByNumberOfCards(cards)
      winners must beEqualTo(Seq(1, 3))
    }
  }

}
