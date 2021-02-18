package part2actors

import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import part2actors.ActorCapabilities.BankClient.LiveTheLife

object ActorCapabilities extends App {

  object SimpleActor{
    def props: Props = Props(new SimpleActor())
  }

  class SimpleActor extends Actor{
    override def receive: Receive = {
      case "Hi!" => context.sender ! "Hello, there!"
      case message: String => println(s"[${context.self}] I have received: $message")
      case number: Int => println(s"[simple actor] I have received a number: $number")
      case SpecialMessage(content) => println(s"[simple actor] I have received something special: $content")
      case SendMessageToYourself(content) => self ! content
      case SayHiTo(reference) => reference ! "Hi!"
      case WirelessPhoneMessage(content, ref) => ref.forward(content + " -s")
    }
  }

  val system = ActorSystem("actorCapabilitiesDemo")
  val simpleActor = system.actorOf(Props[SimpleActor], "simpleActor")

  simpleActor ! "hello, actor"
  simpleActor ! 42

  case class SpecialMessage(contents: String)
  simpleActor ! SpecialMessage("special content")

  case class SendMessageToYourself(content: String)
  simpleActor ! SendMessageToYourself("I'm an actor")

  val alice = system.actorOf(SimpleActor.props, "Alice")
  val bob = system.actorOf(SimpleActor.props, "Bob")

  case class SayHiTo(ref: ActorRef)
  alice ! SayHiTo(bob)

//  alice ! "Hi!"

  case class WirelessPhoneMessage(content: String, ref: ActorRef)
  alice ! WirelessPhoneMessage("Hi!", bob)

  /**
   * Exercises
   * 1. a Counter actor
   *  - Increment
   *  - Decrement
   *  - Print
   *
   * 2. a Bank account as an actor
   *  receives:
   *  - Deposit an amount
   *  - Withdraw an amount
   *  - Statement
   *  sends:
   *  - Success
   *  - Failure
   */

  object CounterActor {
    case object Increment
    case object Decrement
    case object Print
  }

  class CounterActor extends Actor{
    import CounterActor._

    var count: Int = 0

    override def receive: Receive = {
      case Increment => count += 1
      case Decrement => count -= 1
      case Print => println(s"[counter actor] current count is: $count")
    }
  }

  import CounterActor._
  val counterActor = system.actorOf(Props[CounterActor], "counterActor")
  for(_ <- 1 until 10) {counterActor ! Increment}
  for(_ <- 1 until 7) {counterActor ! Decrement}
  counterActor ! Print




  object BankAccount{
    case class Deposit(amount: Int)
    case class Withdraw(amount: Int)
    case object Statement

    case class Success(message: String)
    case class Failure(reason: String)
  }

  class BankAccount extends Actor {
    import BankAccount._
    var currentAmount = 0

    override def receive: Receive = {
      case Deposit(amount) =>
        if(amount < 0 ) sender() ! Failure(s"Invalid deposit $amount")
        else {
          currentAmount += amount
          sender() ! Success(s"Successfully deposited $amount")
        }

      case Withdraw(amount) =>
        if(currentAmount < 0) sender() ! Failure("Invalid withdraw amount")
        else if (amount > currentAmount) sender() ! Failure("Insufficient funds")
        else {
          currentAmount -= amount
          sender() ! Success(s"Successfully withdrew $amount")
        }

      case Statement => sender() ! s"Your balance is $currentAmount"
    }
  }

  object BankClient {
    case class LiveTheLife(account: ActorRef)
  }

  class BankClient extends Actor {
    import BankAccount._
    import BankClient._

    override def receive: Receive = {
      case LiveTheLife(account) =>
        account ! Deposit(10000)
        account ! Withdraw(90000)
        account ! Withdraw(500)
        account ! Statement
      case message => println(message.toString)
    }
  }
  val bankAccount = system.actorOf(Props[BankAccount], "bankAccount")
  val bankClient = system.actorOf(Props[BankClient], "bankClient")

  bankClient ! LiveTheLife(bankAccount)
}
