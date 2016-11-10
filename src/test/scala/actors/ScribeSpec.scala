package actors

import akka.testkit.TestActorRef
import org.junit.runner.RunWith
import org.specs2.mutable.Specification
import org.specs2.runner.JUnitRunner
import protocol._

@RunWith(classOf[JUnitRunner])
class ScribeSpec extends Specification {

  "The Scribe" should {
    "report winner" in new AkkaTestkitSpecs2 {
      val actorRef = TestActorRef[Scribe]

      actorRef ! Winner("me")
    }

  }

}
