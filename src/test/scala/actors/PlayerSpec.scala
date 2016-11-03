package actors

import akka.testkit.TestActorRef
import org.junit.runner.RunWith
import org.specs2.mutable.Specification
import org.specs2.runner.JUnitRunner
import protocol._

@RunWith(classOf[JUnitRunner])
class PlayerSpec extends Specification {

  "The Player" should {
    "receive and give cards" in new AkkaTestkitSpecs2 {
      val actorRef = TestActorRef[Player]
      val actor = actorRef.underlyingActor
      val c1 = Card(Rank.EIGHT, Colour.CLUBS)
      val c2 = Card(Rank.JACK, Colour.HEARTS)
      actorRef ! GiveCard(c1)
      actorRef ! GiveCard(c2)
      actor.receivingCards must haveSize(2)
      actorRef ! AskForCard
      expectMsgType[GiveCard]
      // receiving cards were copied cards queue
      // the above one was already returned
      actor.receivingCards must haveSize(0)
      actor.cards must haveSize(1)
      actorRef ! AskForCard
      expectMsgType[GiveCard]
      actor.receivingCards must haveSize(0)
      actor.cards must haveSize(0)
    }

    "send NoCard when has no cards" in new AkkaTestkitSpecs2 {
      val actorRef = TestActorRef[Player]
      actorRef ! AskForCard
      expectMsg(NoCard)
    }

    "count cards" in new AkkaTestkitSpecs2 {
      val actorRef = TestActorRef[Player]
      actorRef ! CountCards
      expectMsg(NumberOfCards(0))
      actorRef.underlyingActor.receivingCards.add(Card(Rank.ACE, Colour.CLUBS))
      actorRef.underlyingActor.receivingCards.add(Card(Rank.KING, Colour.CLUBS))
      actorRef.underlyingActor.cards.offer(Card(Rank.EIGHT, Colour.CLUBS))
      actorRef ! CountCards
      expectMsg(NumberOfCards(3))
    }
  }

}
