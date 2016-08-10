package com.misoul

import akka.actor.{ ActorRef, ActorSystem, Props, Actor, Inbox }
import scala.concurrent.duration._

case object ASendState
case class AChangeState(data: String)
case class BForwardData(data: String)
case class BPrintData(data: String)
case class MessageC1(data: String)

class ServerA extends Actor {
  private var _state = "UN_INIT"

  def state = _state
  def state_= (state: String): Unit = _state = state

  def receive = {
    case ASendState => { println("Sender info: " + sender.path); sender ! BPrintData(_state) }
    case AChangeState(data) => doActionA2(data)
  }

  private def doActionA2(data: String) = state = data
}

class ServerB extends Actor {
  def receive = {
    case BForwardData(data) => sender ! MessageC1("ServerB_" + data)
    case BPrintData(data) => println("ServerB_" + data)
  }
}

object TryAkka extends App {
  val system = ActorSystem("TryAkka")
  val serverA = system.actorOf(Props[ServerA], "serverA")
  val serverB = system.actorOf(Props[ServerB], "serverB")
  val inbox = Inbox.create(system)

  serverA.tell(AChangeState("State_A_01"), ActorRef.noSender)
  inbox.send(serverA, ASendState)

  val BPrintData(dataB1) = inbox.receive(5.seconds)
  println("ServerA returns: " + dataB1)

  // serverB.tell(BForwardData(data), ActorRef.noSender)

  // inbox.send(serverB, BForwardData(dataB1))
  // val MessageC1(dataC1) = inbox.receive(5.seconds)
  // println("ServerB returns: " + dataC1)

  system.scheduler.schedule(0.seconds, 1.second, serverA, ASendState)(system.dispatcher, serverB)

  // TODO: have serverA, serverB and their clients on separate threads
  

}
