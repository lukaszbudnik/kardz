package actors

import akka.actor.{Actor, ActorLogging}
import protocol.Winner

class Scribe extends Actor with ActorLogging {
  override def receive: Receive = {
    case Winner(name) => {
      log.info("And the winner is {}", name)
    }
  }
}
