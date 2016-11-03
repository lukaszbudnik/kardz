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

      "decrement active players when NoCard is sent" in new AkkaTestkitSpecs2 {
        val p1 = TestProbe()
        val p2 = TestProbe()
        val p3 = TestProbe()

        val refereeRef = TestActorRef(Props(new Referee(Seq(p1.ref, p2.ref, p3.ref), testActor)))

        println(refereeRef)

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

        println(refereeRef)

        // simulate players 1 & 2 ran out of cards
        refereeRef.tell(NoCard, p1.ref)
        refereeRef.tell(NoCard, p2.ref)

        // player 3 won (index 2)
        listener.expectMsg(Winner(2))
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

      "ask to count cards if max rounds got" in new AkkaTestkitSpecs2 {
        val p1 = TestProbe()
        val p2 = TestProbe()
        val p3 = TestProbe()
        val listener = TestProbe()

        val refereeRef = TestActorRef(Props(new Referee(Seq(p1.ref, p2.ref, p3.ref), listener.ref)))

        // simulate players 1 & 2 ran out of cards
        // explicit ! method invocation with sender param
        val c1 = Card(Rank.EIGHT, Colour.CLUBS)
        val c2 = Card(Rank.FIVE, Colour.CLUBS)
        val c3 = Card(Rank.JACK, Colour.CLUBS)
        refereeRef.tell(GiveCard(c1), p1.ref)
        refereeRef.tell(GiveCard(c2), p2.ref)
        refereeRef.tell(GiveCard(c3), p3.ref)



      }

    }

  }

