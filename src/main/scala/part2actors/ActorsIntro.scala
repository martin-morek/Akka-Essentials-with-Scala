package part2actors

import akka.actor.{Actor, ActorRef, ActorSystem, Props}

object ActorsIntro extends App {

  // part-1 actor system
  val actorSystem = ActorSystem("firstActorSystem")
  println(actorSystem.name)

  // part-2 creating actors
  class WordCountActor extends Actor{
    var totalWords = 0

    override def receive: PartialFunction[Any, Unit] = {
      case message: String =>
        println(s"[word counter] I have received: $message")
        totalWords + message.split(" ").length
      case msg => println(s"[word counter] I cannot understand ${msg.toString}")
    }
  }

  // part-3 instantiate actor
  val wordCounter: ActorRef = actorSystem.actorOf(Props[WordCountActor], "wordCounter")
  val anotherWordCounter: ActorRef = actorSystem.actorOf(Props[WordCountActor], "anotherWordCounter")

  // part-4 communication
  wordCounter ! "I'm learning Akka and it's pretty damn cool!"
  anotherWordCounter ! "Different message"

  object Person {
    def props(name: String): Props = Props(new Person(name))
  }

  class Person(name: String) extends Actor {
    override def receive: Receive = {
      case "hi" => println(s"Hi, my name is $name")
      case _ =>
    }
  }

  val person = actorSystem.actorOf(Person.props("Tom"))

  person ! "hi"

}
