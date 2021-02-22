package part2actors

import akka.actor.{Actor, ActorRef, ActorSystem, Props}

object ChildActorsExercise extends App {

  // Distributed word counting
  object WordCounterMaster {
    case class Initialize(nChildren: Int)
    case class WordCountTask(text: String, requester: ActorRef)
    case class WordCountReply(count: Int, requester: ActorRef)
  }

  class WordCounterMaster extends Actor {
    import WordCounterMaster._

    override def receive: Receive = {
      case Initialize(nChildren) =>
        println("[master] initializing...")
        val children: List[ActorRef] = (1 to nChildren).map(_ => context.actorOf(Props[WordCounterWorker])).toList
        context.become(initialized(children, 0))
    }

    def initialized(children: List[ActorRef], current: Int): Receive = {
      case text: String =>
        println(s"[master] I have received : $text - I will send it to $current worker")
        children(current) ! WordCountTask(text, sender())
        val nextWorker = (current + 1) % children.length
        context.become(initialized(children, nextWorker))
      case WordCountReply(count, requester) => requester ! count
    }
  }

  class WordCounterWorker extends Actor {
    import WordCounterMaster._

    override def receive: Receive = {
      case WordCountTask(text, requester) =>
        println(s"[worker] I have received text $text")
        sender() ! WordCountReply(text.split(" ").length, requester)
    }
  }

  class TestActor extends Actor {
    import WordCounterMaster._

    override def receive: Receive = {
      case "go" =>
        val master = context.actorOf(Props[WordCounterMaster], "master")
        master ! Initialize(3)
        val texts = List("I love Akka", "Scala is super dope", "yes", "me too")
        texts.foreach(text => master ! text)
      case count: Int =>
        println(s"[test actor] I received a reply: $count")
    }
  }

  val system = ActorSystem("roundRobinWordCountExercise")
  val testActor = system.actorOf(Props[TestActor], "testActor")
  testActor ! "go"
}
