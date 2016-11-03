package actors

import java.util
import java.util.Collections
import java.util.concurrent.LinkedBlockingQueue

import akka.actor.Actor
import protocol._

class Player extends Actor {
  val receivingCards: util.List[Card] = new util.ArrayList()
  val cards: util.Queue[Card] = new LinkedBlockingQueue()

  override def receive: Receive = {
    case GiveCard(c) => receivingCards.add(c)
    case CountCards => sender ! NumberOfCards(receivingCards.size + cards.size)
    case AskForCard if cards.size == 0 && receivingCards.size == 0 => {
      sender ! NoCard
    }
    case AskForCard => {

      // if cards is empty check receivingCards
      if (cards.size == 0) {
        Collections.shuffle(receivingCards)
        cards.addAll(receivingCards)
        receivingCards.clear
      }

      sender ! GiveCard(cards.poll())
    }

  }
}
