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
      cards.foreach(println(_))
      cards must haveSize(52)
    }

    "return winner for list of cards" in {
      val card1 = Card(Rank.EIGHT, Colour.CLUBS)
      val card2 = Card(Rank.QUEEN, Colour.HEARTS)
      val card3 = Card(Rank.ACE, Colour.SPADES)
      val card4 = Card(Rank.TWO, Colour.DIAMONDS)
      val cards = Seq(card1, card2, card3, card4)
      cards.foreach(println(_))
      val winner = GameEngine.winnerByCards(cards)
      winner must beEqualTo(Seq(2))
    }

    "return winners for list of same cards" in {
      val card1 = Card(Rank.ACE, Colour.CLUBS)
      val card2 = Card(Rank.QUEEN, Colour.HEARTS)
      val card3 = Card(Rank.ACE, Colour.SPADES)
      val card4 = Card(Rank.ACE, Colour.DIAMONDS)
      val cards = Seq(card1, card2, card3, card4)
      cards.foreach(println(_))
      val winner = GameEngine.winnerByCards(cards)
      winner must beEqualTo(Seq(0,2,3))
    }

    "return winner for list of number of cards" in {
      val cards = Seq(12, 23, 3, 6)
      cards.foreach(println(_))
      val winner = GameEngine.winnerByNumberOfCards(cards)
      winner must beEqualTo(1)
    }
  }

}
