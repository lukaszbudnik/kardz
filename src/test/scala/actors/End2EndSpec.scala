package actors

import akka.actor.Props
import org.junit.runner.RunWith
import org.specs2.mutable.Specification
import org.specs2.runner.JUnitRunner
import protocol._
import scala.concurrent.duration._

@RunWith(classOf[JUnitRunner])
class End2EndSpec extends Specification {

  skipAllIf(Option(System.getenv("TRAVIS")).isDefined)

  "The Kardz" should {
    "play game" in new AkkaTestkitSpecs2 {
      val playerProps = Props[Player]
      val p1 = system.actorOf(playerProps)
      val p2 = system.actorOf(playerProps)
      val p3 = system.actorOf(playerProps)

      val scribeProps = Props[Scribe]
      val scribe = system.actorOf(scribeProps)

      val refereeProps = Props(new Referee(Seq(p1, p2, p3), scribe))
      val refereeRef = system.actorOf(refereeProps)

      refereeRef ! Distribute
      refereeRef ! Start

      Thread.sleep(5000)
    }
  }

}
