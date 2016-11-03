package actors

import akka.actor.Actor
import protocol.Winner

class Scribe extends Actor {
  override def receive: Receive = {
    case Winner(i) => {
      println(s"And the winner is $i")
      context.system.terminate
    }
  }
}