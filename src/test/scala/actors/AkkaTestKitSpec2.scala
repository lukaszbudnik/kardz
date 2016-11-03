package actors

import akka.actor.ActorSystem
import akka.testkit.{TestKit, ImplicitSender}
import org.specs2.mutable.After

abstract class AkkaTestkitSpecs2 extends TestKit(ActorSystem())
with After
with ImplicitSender {
  def after = system.terminate()
}
