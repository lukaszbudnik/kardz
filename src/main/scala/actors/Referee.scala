package actors

import akka.actor.{ActorRef, Actor, ActorLogging, OneForOneStrategy, SupervisorStrategy}
import akka.actor.SupervisorStrategy._
import engine.GameEngine
import protocol._

import scala.collection.mutable
import scala.concurrent.duration._

class Referee(players: Seq[ActorRef], listener: ActorRef) extends Actor with ActorLogging {

  val maxRounds = 500

  var activePlayers: Seq[ActorRef] = players
  var currentRound: mutable.Map[ActorRef, Seq[Card]] = mutable.Map()
  var cardsCount: mutable.Map[ActorRef, Int] = mutable.Map()
  var roundCounter = 0
  var atWarCounter = 0
  var currentWarRound: mutable.Map[ActorRef, Int] = mutable.Map()
  var finished = false

  override def supervisorStrategy: SupervisorStrategy = OneForOneStrategy(maxNrOfRetries = 10, withinTimeRange = 1.second) {
    case _: Exception => Restart
  }

  override def receive: Receive = {
    case Distribute => {
      log.info("About to distribute cards to {}", players)
      val cards: Seq[Card] = GameEngine.distribute
      cards.zipWithIndex.foreach {
        case (c, i) => {
          val player = activePlayers(i % players.size)
          log.debug("Sending {} to {}", c, player)
          player ! GiveCard(c)
        }
      }
    }
    case Start => {
      players.foreach(_ ! AskForCard)
    }
    case NoCard => {
      log.info("Player {} sent NoCard in round {}", sender, roundCounter)
      activePlayers = activePlayers.filterNot(_ == sender)
      if (atWarCounter > 0) {
        currentWarRound += (sender -> 2)
      }
      self ! Check
    }
    case GiveCard(c) => {
      // when at war, Players are sending more cards thus the currentRound is a map of [ActorRef, Seq[Card]]
      if (currentRound.keys.exists(_ == sender)) {
        currentRound += (sender -> currentRound(sender).:+(c))
        log.debug("At war. Player {} sent {}", sender, c)
        // at war Players are expected to send 2 additional Cards - count them
        if (currentWarRound.keys.exists(_ == sender)) {
          currentWarRound += (sender -> 2)
        } else {
          currentWarRound += (sender -> 1)
        }
      } else {
        currentRound += (sender -> Seq(c))
      }
      self ! Check
    }
    case NumberOfCards(n) if roundCounter == maxRounds => {
      cardsCount += (sender -> n)
      if (cardsCount.size == activePlayers.size) {
        log.info("Checking number of cards for {}", cardsCount)
        val indexes = GameEngine.winnerByNumberOfCards(cardsCount.values.toSeq)
        indexes.foreach { i =>
          listener ! Winner(cardsCount.keys.toSeq(i).path.toString)
        }
      }
    }
    case Check if !finished && activePlayers.size == 1 => {
      val winner = activePlayers.head
      log.info("Last player standing {} in round {}", winner, roundCounter)
      finished = true
      listener ! Winner(activePlayers.head.path.toString)
    }
    case Check if !finished && ((activePlayers.size == currentRound.size && atWarCounter == 0) || (currentWarRound.values.sum / 2 == atWarCounter && atWarCounter > 0)) => {
      val cards = currentRound.values.toSeq
      val index = GameEngine.winnerByCards(cards)
      if (index.size > 1) {
        atWarCounter = index.size
        currentWarRound = currentWarRound.empty
        log.info("War in round {} between {}", roundCounter, index.map(currentRound.keys.toSeq(_)))
        index.map(currentRound.keys.toSeq(_)).foreach(p => {
          p ! AskForCard
          p ! AskForCard
        })
      } else {
        val winner = currentRound.keys.toSeq(index.head)
        val cardsFlatten = cards.flatten.toList
        finaliseRound(winner, cardsFlatten)
      }
    }
  }

  def finaliseRound(winner: ActorRef, cards: Seq[Card]): Unit = {
    cards.foreach(winner ! GiveCard(_))
    if (atWarCounter > 0) {
      log.info("War is over. Player {} takes {} cards", winner, cards.size)
    }
    if (roundCounter % 50 == 0) {
      log.debug("Round {} winner {} takes {}", roundCounter, winner, cards)
    }
    val action = if (roundCounter == maxRounds) {
      CountCards
    } else {
      roundCounter += 1
      AskForCard
    }
    activePlayers.foreach(_ ! action)
    currentRound = currentRound.empty
    currentWarRound = currentWarRound.empty
    atWarCounter = 0
  }

}
