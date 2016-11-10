package actors

import akka.actor.Props
import akka.testkit.{TestProbe, TestActorRef}
import org.junit.runner.RunWith
import org.specs2.mutable.Specification
import org.specs2.runner.JUnitRunner
import protocol._

@RunWith(classOf[JUnitRunner])
class RefereeSpec extends Specification {

    "The Referee" should {
      "distribute cards" in new AkkaTestkitSpecs2 {
        val p1 = TestProbe()
        val p2 = TestProbe()
        val p3 = TestProbe()
        val refereeRef = TestActorRef(Props(new Referee(Seq(p1.ref, p2.ref, p3.ref), testActor)))

        refereeRef ! Distribute

        // 52 = 18 + 17 + 17
        p1.receiveN(18)
        p2.receiveN(17)
        p3.receiveN(17)

      }

      "start game" in new AkkaTestkitSpecs2 {
        val p1 = TestProbe()
        val p2 = TestProbe()
        val p3 = TestProbe()
        val refereeRef = TestActorRef(Props(new Referee(Seq(p1.ref, p2.ref, p3.ref), testActor)))

        refereeRef ! Start

        p1.expectMsg(AskForCard)
        p2.expectMsg(AskForCard)
        p3.expectMsg(AskForCard)
      }

      "ask to count cards when max round reached" in new AkkaTestkitSpecs2 {
        val p1 = TestProbe()
        val p2 = TestProbe()
        val p3 = TestProbe()
        val refereeRef = TestActorRef(Props(new Referee(Seq(p1.ref, p2.ref, p3.ref), testActor)))

        val referee = refereeRef.underlyingActor.asInstanceOf[Referee]
        referee.roundCounter = referee.maxRounds

        val c1 = Card(Rank.EIGHT, Colour.CLUBS)
        val c2 = Card(Rank.FIVE, Colour.CLUBS)
        val c3 = Card(Rank.JACK, Colour.CLUBS)

        refereeRef.tell(GiveCard(c1), p1.ref)
        refereeRef.tell(GiveCard(c2), p2.ref)
        refereeRef.tell(GiveCard(c3), p3.ref)

        p1.expectMsg(CountCards)
        p2.expectMsg(CountCards)
        // player 3 won current round got 3 cards and then was asked to count cards
        p3.receiveN(3)
        p3.expectMsg(CountCards)
      }

      "send Winner with the most cards to listener when max rounds reached" in new AkkaTestkitSpecs2 {
        val p1 = TestProbe()
        val p2 = TestProbe()
        val p3 = TestProbe()
        val listener = TestProbe()

        val refereeRef = TestActorRef(Props(new Referee(Seq(p1.ref, p2.ref, p3.ref), listener.ref)))
        val referee = refereeRef.underlyingActor.asInstanceOf[Referee]
        referee.roundCounter = referee.maxRounds

        refereeRef.tell(NumberOfCards(17), p1.ref)
        refereeRef.tell(NumberOfCards(17), p2.ref)
        refereeRef.tell(NumberOfCards(18), p3.ref)

        listener.expectMsg(Winner(p3.ref.path.toString))
      }

      "send Winners with the most cards to listener when max rounds reached" in new AkkaTestkitSpecs2 {
        val p1 = TestProbe()
        val p2 = TestProbe()
        val p3 = TestProbe()
        val listener = TestProbe()

        val refereeRef = TestActorRef(Props(new Referee(Seq(p1.ref, p2.ref, p3.ref), listener.ref)))
        val referee = refereeRef.underlyingActor.asInstanceOf[Referee]
        referee.roundCounter = referee.maxRounds

        refereeRef.tell(NumberOfCards(26), p1.ref)
        refereeRef.tell(NumberOfCards(0), p2.ref)
        refereeRef.tell(NumberOfCards(26), p3.ref)


        // messages are stored in set so in case of draw p1 or p3 can be sent in different order
        listener.expectMsgPF() {
          case Winner(name) if p1.ref.path.toString == name || p3.ref.path.toString == name => true
        }
        listener.expectMsgPF() {
          case Winner(name) if p1.ref.path.toString == name || p3.ref.path.toString == name => true
        }
      }

      "decrement active players when NoCard is sent" in new AkkaTestkitSpecs2 {
        val p1 = TestProbe()
        val p2 = TestProbe()
        val p3 = TestProbe()

        val refereeRef = TestActorRef(Props(new Referee(Seq(p1.ref, p2.ref, p3.ref), testActor)))

        // simulate player 2 ran out of cards
        // explicit ! method invocation with sender param
        refereeRef.!(NoCard)(p2.ref)

        refereeRef.underlyingActor.asInstanceOf[Referee].activePlayers must haveSize(2)
      }

      "send Winner to listener if only 1 active players remains" in new AkkaTestkitSpecs2 {
        val p1 = TestProbe()
        val p2 = TestProbe()
        val p3 = TestProbe()
        val listener = TestProbe()

        val refereeRef = TestActorRef(Props(new Referee(Seq(p1.ref, p2.ref, p3.ref), listener.ref)))

        // simulate players 1 & 2 ran out of cards
        refereeRef.tell(NoCard, p1.ref)
        refereeRef.tell(NoCard, p2.ref)

        refereeRef.tell(GiveCard(Card(Rank.TWO, Colour.DIAMONDS)), p3.ref)

        // player 3 won
        listener.expectMsg(Winner(p3.ref.path.toString))
      }

      "receive cards & send results to round winner" in new AkkaTestkitSpecs2 {
        val p1 = TestProbe()
        val p2 = TestProbe()
        val p3 = TestProbe()
        val listener = TestProbe()

        val refereeRef = TestActorRef(Props(new Referee(Seq(p1.ref, p2.ref, p3.ref), listener.ref)))

        val c1 = Card(Rank.EIGHT, Colour.CLUBS)
        val c2 = Card(Rank.FIVE, Colour.CLUBS)
        val c3 = Card(Rank.JACK, Colour.CLUBS)

        // explicit tell method invocation with sender param
        refereeRef.tell(GiveCard(c1), p1.ref)
        refereeRef.tell(GiveCard(c2), p2.ref)
        refereeRef.tell(GiveCard(c3), p3.ref)

        // player 3 won round - should get 3 cards
        p3.expectMsgType[GiveCard]
        p3.expectMsgType[GiveCard]
        p3.expectMsgType[GiveCard]
        p3.expectMsg(AskForCard)

        p1.expectMsg(AskForCard)
        p2.expectMsg(AskForCard)
      }

      "receive cards & ask for two more cards when war" in new AkkaTestkitSpecs2 {
        val p1 = TestProbe()
        val p2 = TestProbe()
        val p3 = TestProbe()
        val listener = TestProbe()

        val refereeRef = TestActorRef(Props(new Referee(Seq(p1.ref, p2.ref, p3.ref), listener.ref)))

        val c1 = Card(Rank.JACK, Colour.HEARTS)
        val c2 = Card(Rank.FIVE, Colour.DIAMONDS)
        val c3 = Card(Rank.JACK, Colour.CLUBS)

        // explicit tell method invocation with sender param
        refereeRef.tell(GiveCard(c1), p1.ref)
        refereeRef.tell(GiveCard(c2), p2.ref)
        refereeRef.tell(GiveCard(c3), p3.ref)

        // player 1 and player 3 - war
        // referee asks for 2 more cards
        p1.expectMsg(AskForCard)
        p1.expectMsg(AskForCard)

        p3.expectMsg(AskForCard)
        p3.expectMsg(AskForCard)

        p2.expectNoMsg
      }

      "play war rounds" in new AkkaTestkitSpecs2 {
        val p1 = TestProbe()
        val p2 = TestProbe()
        val p3 = TestProbe()
        val listener = TestProbe()

        val refereeRef = TestActorRef[Referee](Props(new Referee(Seq(p1.ref, p2.ref, p3.ref), listener.ref)))

        val c11 = Card(Rank.JACK, Colour.HEARTS)
        val c21 = Card(Rank.JACK, Colour.DIAMONDS)
        val c31 = Card(Rank.JACK, Colour.CLUBS)

        // explicit tell method invocation with sender param
        refereeRef.tell(GiveCard(c11), p1.ref)
        refereeRef.tell(GiveCard(c21), p2.ref)
        refereeRef.tell(GiveCard(c31), p3.ref)

        // player 1, 2, and 3 all sent Jacks
        // referee asks for 2 more cards
        p1.expectMsg(AskForCard)
        p1.expectMsg(AskForCard)

        p2.expectMsg(AskForCard)
        p2.expectMsg(AskForCard)

        p3.expectMsg(AskForCard)
        p3.expectMsg(AskForCard)

        // first cards at war are hidden, they don't count
        val c12a = Card(Rank.SIX, Colour.HEARTS)
        val c22a = Card(Rank.TEN, Colour.DIAMONDS)
        val c32a = Card(Rank.EIGHT, Colour.CLUBS)

        refereeRef.tell(GiveCard(c12a), p1.ref)
        refereeRef.tell(GiveCard(c22a), p2.ref)
        refereeRef.tell(GiveCard(c32a), p3.ref)

        // second cards count - make it war one more time
        val c12b = Card(Rank.SEVEN, Colour.HEARTS)
        val c22b = Card(Rank.SEVEN, Colour.DIAMONDS)
        val c32b = Card(Rank.SEVEN, Colour.CLUBS)

        refereeRef.tell(GiveCard(c12b), p1.ref)
        refereeRef.tell(GiveCard(c22b), p2.ref)
        refereeRef.tell(GiveCard(c32b), p3.ref)

        // player 1, 2, and 3 all sent Sevens
        // referee asks for 2 more cards
        p1.expectMsg(AskForCard)
        p1.expectMsg(AskForCard)

        p2.expectMsg(AskForCard)
        p2.expectMsg(AskForCard)

        p3.expectMsg(AskForCard)
        p3.expectMsg(AskForCard)

        // first cards at war are hidden, they don't count
        val c13a = Card(Rank.QUEEN, Colour.HEARTS)
        val c23a = Card(Rank.KING, Colour.DIAMONDS)
        val c33a = Card(Rank.ACE, Colour.CLUBS)

        refereeRef.tell(GiveCard(c13a), p1.ref)
        refereeRef.tell(GiveCard(c23a), p2.ref)
        refereeRef.tell(GiveCard(c33a), p3.ref)

        // second cards count - make it war one more time
        val c13b = Card(Rank.SEVEN, Colour.HEARTS)
        val c23b = Card(Rank.SIX, Colour.DIAMONDS)
        val c33b = Card(Rank.SEVEN, Colour.CLUBS)

        refereeRef.tell(GiveCard(c13b), p1.ref)
        refereeRef.tell(GiveCard(c23b), p2.ref)
        refereeRef.tell(GiveCard(c33b), p3.ref)

        // players 1 and 3 at war - referee asks for 2 more cards
        p1.expectMsg(AskForCard)
        p1.expectMsg(AskForCard)
        p3.expectMsg(AskForCard)
        p3.expectMsg(AskForCard)

        // player 2 lost - no message
        p2.expectNoMsg

        // why not this is a long test...
        // player 1 and 3 at war another draw...

        // first cards at war are hidden, they don't count
        val c14a = Card(Rank.TWO, Colour.HEARTS)
        val c34a = Card(Rank.THREE, Colour.CLUBS)

        refereeRef.tell(GiveCard(c14a), p1.ref)
        refereeRef.tell(GiveCard(c34a), p3.ref)

        // second cards count - make it war one more time
        val c14b = Card(Rank.NINE, Colour.HEARTS)
        val c34b = Card(Rank.NINE, Colour.CLUBS)

        refereeRef.tell(GiveCard(c14b), p1.ref)
        refereeRef.tell(GiveCard(c34b), p3.ref)

        // players 1 and 3 at war - referee asks for 2 more cards
        p1.expectMsg(AskForCard)
        p1.expectMsg(AskForCard)
        p3.expectMsg(AskForCard)
        p3.expectMsg(AskForCard)

        // finally player 3 wins

        // first cards at war are hidden, they don't count
        val c15a = Card(Rank.THREE, Colour.HEARTS)
        val c35a = Card(Rank.FOUR, Colour.CLUBS)

        refereeRef.tell(GiveCard(c15a), p1.ref)
        refereeRef.tell(GiveCard(c35a), p3.ref)

        // second cards count - make it war one more time
        val c15b = Card(Rank.FOUR, Colour.HEARTS)
        val c35b = Card(Rank.FIVE, Colour.CLUBS)

        refereeRef.tell(GiveCard(c15b), p1.ref)
        refereeRef.tell(GiveCard(c35b), p3.ref)

        // p3 gets all cards
        p3.receiveWhile(messages = 23) {
          case GiveCard(c) => true
        }

        // game resumes referee asks player 1, 2, and 3 for new cards
        p1.expectMsg(AskForCard)
        p2.expectMsg(AskForCard)
        p3.expectMsg(AskForCard)

        // round counter is incremented
        refereeRef.underlyingActor.roundCounter must beEqualTo(1)
      }

      "play war rounds with NoCard" in new AkkaTestkitSpecs2 {
        val p1 = TestProbe()
        val p2 = TestProbe()
        val p3 = TestProbe()
        val listener = TestProbe()

        val refereeRef = TestActorRef[Referee](Props(new Referee(Seq(p1.ref, p2.ref, p3.ref), listener.ref)))

        val c11 = Card(Rank.JACK, Colour.HEARTS)
        val c21 = Card(Rank.JACK, Colour.DIAMONDS)
        val c31 = Card(Rank.JACK, Colour.CLUBS)

        // explicit tell method invocation with sender param
        refereeRef.tell(GiveCard(c11), p1.ref)
        refereeRef.tell(GiveCard(c21), p2.ref)
        refereeRef.tell(GiveCard(c31), p3.ref)

        // player 1, 2, and 3 all sent Jacks
        // referee asks for 2 more cards
        p1.expectMsg(AskForCard)
        p1.expectMsg(AskForCard)

        p2.expectMsg(AskForCard)
        p2.expectMsg(AskForCard)

        p3.expectMsg(AskForCard)
        p3.expectMsg(AskForCard)

        // first cards at war are hidden, they don't count
        val c12a = Card(Rank.SIX, Colour.HEARTS)
        val c22a = Card(Rank.TEN, Colour.DIAMONDS)
        val c32a = Card(Rank.EIGHT, Colour.CLUBS)

        refereeRef.tell(GiveCard(c12a), p1.ref)
        refereeRef.tell(GiveCard(c22a), p2.ref)
        refereeRef.tell(GiveCard(c32a), p3.ref)

        // p1 ran out of cards
        refereeRef.tell(NoCard, p1.ref)

        // second cards count - make it war one more time
        val c22b = Card(Rank.SEVEN, Colour.DIAMONDS)
        val c32b = Card(Rank.SEVEN, Colour.CLUBS)

        refereeRef.tell(GiveCard(c22b), p2.ref)
        refereeRef.tell(GiveCard(c32b), p3.ref)

        // player 2, and 3 sent Sevens
        // referee asks for 2 more cards
        p2.expectMsg(AskForCard)
        p2.expectMsg(AskForCard)

        p3.expectMsg(AskForCard)
        p3.expectMsg(AskForCard)

        // player 1 lost
        p1.expectNoMsg

        // only 2 active players remain
        refereeRef.underlyingActor.activePlayers must haveSize(2)
      }

    }

  }
