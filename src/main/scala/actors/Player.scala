package actors

import java.util
import java.util.Collections
import java.util.concurrent.LinkedBlockingQueue

import akka.actor.{Actor, ActorLogging}
import protocol._

case class State(receivingCards: util.List[Card], cards: util.Queue[Card])

object Persistence {

  val data: util.Map[String, State] = new util.HashMap[String, State]()

  def save(actorPath: String, state: State): Unit = {
    data.put(actorPath, state)
  }

  def load(actorPath: String): Option[State] = {
    Some(data.get(actorPath))
  }

}

class Player extends Actor with ActorLogging {
  var receivingCards: util.List[Card] = new util.ArrayList()
  var cards: util.Queue[Card] = new LinkedBlockingQueue()

  override def receive: Receive = {
    case GiveCard(c) => receivingCards.add(c)
    case CountCards => sender ! NumberOfCards(receivingCards.size + cards.size)
    case AskForCard if cards.size == 0 && receivingCards.size == 0 => {
      sender ! NoCard
    }
    case AskForCard => {

      val r = new java.util.Random()
      if (r.nextInt(50) == 49) {
        throw new RuntimeException("kaboom")
      }

      // if cards is empty check receivingCards
      if (cards.size == 0) {
        Collections.shuffle(receivingCards)
        cards.addAll(receivingCards)
        receivingCards.clear
      }

      sender ! GiveCard(cards.poll())
    }

  }

  override def preRestart(reason: Throwable, message: Option[Any]): Unit = {
    log.info("pre-restarting with message {} from {} with receiving cards {} and cards {}", message, sender, receivingCards.size, cards.size)
    message match {
      case Some(m) => {
        Persistence.save(self.path.toString, State(receivingCards, cards))
        self.tell(m, sender)
      }
      case _ =>
    }
  }

  override def postRestart(reason: Throwable): Unit = {
    val state = Persistence.load(self.path.toString)
    state match {
      case Some(s) => {
        receivingCards = s.receivingCards
        cards = s.cards
      }
      case _ =>
    }
    log.info("post-restarting with receiving cards {} and cards {}", receivingCards.size, cards.size)
  }

}
