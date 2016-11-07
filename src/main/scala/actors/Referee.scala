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
      log.info("About to distribute cards to {} players", players.size)
      val cards: Seq[Card] = GameEngine.distribute
      for (i <- 0 until cards.size) {
        val player = activePlayers(i % players.size)
        val card = cards(i)
        log.debug("Sending {} to {}", card, player)
        player ! GiveCard(card)
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
      if (currentRound.keys.exists(_ == sender)) {
        currentRound += (sender -> currentRound(sender).:+(c))
      } else {
        currentRound += (sender -> Seq(c))
      }
      if (atWarCounter > 0) {
        log.debug("At war. Player {} sent {}", sender, c)
        if (currentWarRound.keys.exists(_ == sender)) {
          currentWarRound += (sender -> 2)
        } else {
          currentWarRound += (sender -> 1)
        }
      }

      self ! Check
    }
    case NumberOfCards(n) => {
      cardsCount += (sender -> n)
      if (cardsCount.size == activePlayers.size) {
        log.info("Checking number of cards for {}", cardsCount)
        val indexes = GameEngine.winnerByNumberOfCards(cardsCount.values.toSeq)
        indexes.foreach { i =>
          listener ! Winner(cardsCount.keys.toSeq(i).path.toString)
        }
      }
    }
    case Check if !finished && ((activePlayers.size == currentRound.size && atWarCounter == 0) || (currentWarRound.values.sum / 2 == atWarCounter && atWarCounter > 0)) => {
      val cards = currentRound.values.toSeq

      val index = GameEngine.winnerByCards(cards)

      if (index.size > 1) {
        atWarCounter = index.size
        currentWarRound = currentWarRound.empty
        log.info("War in round {} between {}", roundCounter, currentRound.keys.zipWithIndex.filter {
          case (p, i) if index.contains(i)  => {
            true
          }
          case _ => false
        }.map {
          case (p, i) => p
        }.toList)
        index.map(currentRound.keys.toSeq(_)).foreach(p => {
          p ! AskForCard
          p ! AskForCard
        })
      } else {
        val winner = currentRound.keys.toSeq(index.head)

        val cardsFlatten = cards.flatten.toList

        if (atWarCounter > 0) {
          log.info("War is over. Player {} takes {}", winner, cardsFlatten)
        }

        if (activePlayers.size == 1) {
          log.info("Last player standing {} in round {}", winner, roundCounter)
          finished = true
          listener ! Winner(activePlayers.head.path.toString)
        } else {
          sendCards(winner, cardsFlatten)
        }
      }
    }
  }

  def sendCards(winner: ActorRef, cards: Seq[Card]): Unit = {

    cards.foreach(c => winner ! GiveCard(c))

    if (roundCounter % 50 == 0) {
      log.info("Round {} winner {} takes {}", roundCounter, winner, cards)
    }

    val action = if (roundCounter == maxRounds) {
      CountCards
    } else {
      AskForCard
    }

    activePlayers.foreach(p => p ! action)

    currentRound = currentRound.empty
    currentWarRound = currentWarRound.empty
    atWarCounter = 0
    roundCounter += 1
  }

}
