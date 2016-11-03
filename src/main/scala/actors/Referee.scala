package actors

import akka.actor.{ActorRef, Actor}
import engine.GameEngine
import protocol._

import scala.collection.mutable

class Referee(players: Seq[ActorRef], listener: ActorRef) extends Actor {

  val maxRounds = 500

  var activePlayers: Seq[ActorRef] = players
  var currentRound: mutable.Map[ActorRef, Card] = mutable.Map()
  var cardsCount: mutable.Map[ActorRef, Int] = mutable.Map()
  var roundCounter = 0

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
      players.foreach(p => p ! AskForCard)
    }
    case NoCard => {
      println(s"The player $sender sent NoCard message in round $roundCounter")
      activePlayers = activePlayers.filterNot(_ == sender)
      if (activePlayers.size == 1) {
        listener ! Winner(players.indexOf(activePlayers.head))
      } else {
        self ! Check
      }
    }
    case GiveCard(c) => {
      currentRound += (sender -> c)
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
    case Check if activePlayers.size == currentRound.size => {
      val cards = currentRound.values.toSeq
      val index = GameEngine.winnerByCards(cards)
      // TODO
      val winner = currentRound.keys.toSeq(index.head)

//      println(s"Player $winner got ${cards.size} cards")

      cards.foreach(c => winner ! GiveCard(c))

      roundCounter += 1

      val action = if (roundCounter == maxRounds) {
        CountCards
      } else {
        AskForCard
      }
//      println(s"Sending $action")
      currentRound.keys.foreach(p => p ! action)
      currentRound = currentRound.empty
    }
  }
}



