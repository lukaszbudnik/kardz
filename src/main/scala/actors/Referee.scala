package actors

import akka.actor.{ActorRef, Actor}
import engine.GameEngine
import protocol._

import scala.collection.mutable

class Referee(players: Seq[ActorRef], listener: ActorRef) extends Actor {

  val maxRounds = 500

  var activePlayers: Seq[ActorRef] = players
  var currentRound: mutable.Map[ActorRef, Seq[Card]] = mutable.Map()
  var cardsCount: mutable.Map[ActorRef, Int] = mutable.Map()
  var roundCounter = 0
  var atWarCounter = 0
  var currentWarRound: mutable.Map[ActorRef, Int] = mutable.Map()

  override def receive: Receive = {
    case Distribute => {
      val cards: Seq[Card] = GameEngine.distribute
      for (i <- 0 until cards.size) {
        val player = activePlayers(i % players.size)
        val card = cards(i)
//        println(s"sending $card to $player")
        player ! GiveCard(card)
      }
    }
    case Start => {
      var i = 0
      players.foreach(p => {
        println(s"$i = $p")
        i += 1
        p ! AskForCard
      })
    }
    case NoCard => {
      println(s"The $sender sent NoCard message in round $roundCounter at war $atWarCounter")

      activePlayers = activePlayers.filterNot(_ == sender)

      if (atWarCounter > 0) {
        currentWarRound += (sender -> 2)
      }

      if (activePlayers.size > 1) {
        self ! Check
      }

    }
    case GiveCard(c) => {
      if (currentRound.keys.exists(_ == sender)) {
        currentRound += (sender -> currentRound(sender).:+(c))
      } else {
        currentRound += (sender -> Seq(c))
      }
      if (atWarCounter > 0) {
//        println(s"At war got card $c from $sender")
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
        println(s"Checking number of cards for $cardsCount")
        val index = GameEngine.winnerByNumberOfCards(cardsCount.values.toSeq)
        listener ! Winner(players.indexOf(activePlayers(index)))
      }
    }
    case Check if (activePlayers.size == currentRound.size && atWarCounter == 0) || (currentWarRound.values.sum / 2 == atWarCounter && atWarCounter > 0) => {
      val cards = currentRound.values.toSeq

      val index = GameEngine.winnerByCards(cards)

      if (index.size > 1) {
        atWarCounter = index.size
        currentWarRound = currentWarRound.empty
        println(s"Round $roundCounter players $index at war!")
        index.map(currentRound.keys.toSeq(_)).foreach(p => {
          p ! AskForCard
          p ! AskForCard
        })
      } else {
        val winner = currentRound.keys.toSeq(index.head)

        if (atWarCounter > 0) {
          println(s"War is over $winner takes ${cards.flatten}")
        }

        sendCards(winner, cards.flatten)
      }
    }
  }

  def sendCards(winner: ActorRef, cards: Seq[Card]): Unit = {

    cards.foreach(c => winner ! GiveCard(c))

    if (roundCounter % 50 == 0) {
      println(s"Round $roundCounter winner $winner taking $cards")
    }

    if (activePlayers.size == 1) {
      println(s"Round $roundCounter last player standing $winner from $sender")
      listener ! Winner(players.indexOf(activePlayers.head))
    } else {

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
}
