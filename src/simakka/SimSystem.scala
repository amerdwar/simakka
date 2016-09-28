package simakka

import java.util.concurrent.atomic.AtomicInteger
import akka.actor.Actor.Receive
import akka.actor.{Actor, ActorLogging, ActorRef, ActorSystem, Props}
import akka.event.LoggingReceive
import simakka.experiments.exp01._
import akka.actor.Deploy
import akka.remote.RemoteScope
import simakka.q.Source
import scala.actors.ActorRef

/**
  * 
  */
class SimSystem extends Actor with SimConstants with ActorLogging with SimEntityLookup {

  val atomicId = new AtomicInteger
  var statCollector:akka.actor.ActorRef = null 
  ////  context.setReceiveTimeout(Duration(100, TimeUnit.MILLISECONDS))


  def addLink(from: Int, to: Int): Unit = {
    log.debug(s"AddLink($from, $to)")

//    val fromEntity = entities.get(from)
    val fromEntity = getRef(from)
    val toEntity = getRef(to)
    if (toEntity == None || fromEntity == None) {
      log.error(s"Either entity $from or $to is not available in entities yet")
    } else {
      toEntity.get ! AddLinkFrom(from, to)
    }
  }


  def creatEntity(tp: String, name: String, id: Int): Props = tp match {
    case "SE" => Props(new SE(name, id))
    //      case "QueueServer"
    case "Counter" => Props( new Counter(name, id))
  
    case "Client" => Props(new Client(name, id))
     case "Responder" =>Props( new Responder(name, id))
    case "ResClient" => Props(new ResClient(name, id))
    case "Source" => Props(new Source(name, id))
    case "Server" => Props(new simakka.q.Server(name, id))
    
    case _ => null
  }
 def creatRemoteEntity(tp: String, name: String, id: Int,url: String): Props ={
   tp match {
    case "SE" =>{ Props(new SE(name, id))}
    //      case "QueueServer"
    case "Counter" =>{
      val one = akka.actor.AddressFromURIString(url)
    
  Props( new Counter(name, id)).withDeploy(Deploy(scope=RemoteScope(one)))
    }
    
     case "Server" =>{
      val one = akka.actor.AddressFromURIString(url)
    
  Props( new simakka.q.Server(name, id)).withDeploy(Deploy(scope=RemoteScope(one)))
    }
        case "Source" =>{
      val one = akka.actor.AddressFromURIString(url)
    
  Props( new simakka.q.Source(name, id)).withDeploy(Deploy(scope=RemoteScope(one)))
    }
    
  case "Responder" =>{
      val one = akka.actor.AddressFromURIString(url)
    
  Props( new Responder(name, id)).withDeploy(Deploy(scope=RemoteScope(one)))
    }
    case "ResClient" =>{
      val one = akka.actor.AddressFromURIString(url)
    
  Props( new ResClient(name, id)).withDeploy(Deploy(scope=RemoteScope(one)))
    }
    
    case "Client" =>{
            val one = akka.actor.AddressFromURIString(url)
 Props(new Client(name, id)).withDeploy(Deploy(scope=RemoteScope(one)))
    }
    case _ => null
  }
 }


  override def receive: Receive = LoggingReceive{

    case s: String => log.debug(s)

    case pb : PockBack => log.debug(pb.toString)

    case ce: CreateEntity =>
      log.debug(s"Create Entity = $ce ")

      val id = atomicId.incrementAndGet()
      val actorRef = context.actorOf(
        creatEntity(ce.tp, ce.name, id), name = ce.name )
      addRef(ce.name, id, actorRef)
      actorRef ! AddRef(ce.name, id, actorRef)
      actorRef ! SetStatCollector(statCollector)
      actorRef ! POCK
   case ce: CreateRemoteEntity =>
      log.debug(s"Create remote Entity = $ce ")

      val id = atomicId.incrementAndGet()
      val actorRef = context.actorOf(
         creatRemoteEntity  (ce.tp, ce.name, id,ce.url), name = ce.name )
      addRef(ce.name, id, actorRef)
      actorRef ! AddRef(ce.name, id, actorRef)
      actorRef ! POCK

    case ie: InitEntity =>
      log.debug(s"Init Entity = $ie")
      val actorRef = getRef(ie.name)
      if(actorRef != None) actorRef.get ! ie

    case aln: AddLinkN =>
      log.debug(aln.toString)
      val from = getId(aln.sFrom).get
      val to = getId(aln.sTo).get

      val fromActor = getRef(from).get
      val toActor = getRef(to).get

      toActor ! AddRef(aln.sFrom, from, fromActor)
//      toActor ! AddRef(aln.sTo, to, toActor) //TODO delet later
      fromActor ! AddRef(aln.sTo, to, toActor)
//      fromActor ! AddRef(aln.sFrom, from, fromActor) // TODO delete later

      toActor ! AddLinkFrom(from, to)
      fromActor ! AddLinkTo(from, to)

  //fromActor  ! AddLinkFrom(to, from)
    //   toActor   ! AddLinkTo(to, from)


    case EndOfSimulation => log.debug(s"end of simulation $EndOfSimulation")
    
    case ss:SetStatCollector=>{
     statCollector=ss.ref;
      
      
    }

    case ev: SimEv => log.debug(s"SimEv=$ev")

    case _ => log.info("Not recognized message")

  }



}

class SE(name: String, id: Int) extends SimEntity(name, id) {

  override def handleEvent(ev: SimEv): Unit = {
    log.info(s"$name: received message : $ev")
    scheduleLocal(5, 55, None)
    scheduleLocal(6, 55, None)
    scheduleLocal(7, 55, None)

  }

  override def initEntity(data: Option[String]): Unit = {
    log.debug(s"$name : ${data.get}")
  }
}

object SimSystem extends SimConstants{
  def startServer(): Unit = {
    val system = ActorSystem("simakka")

    val simSystem = system.actorOf(Props[SimSystem], "SimSystem")

    simSystem ! "Test String Message"
//    simSystem ! EndOfSimulation
    simSystem ! CreateEntity("SE", "entt1")
    sleep(1)

    simSystem ! InitEntity("entt1", Some("""{"key": "value"}"""))
    sleep(1)

    simSystem ! CreateEntity("SE", "entt2")
    sleep(1)

    simSystem ! AddLinkN("entt1", "entt2")
    sleep(1)


//    simSystem ! SimStartTest

    println("before Sleep")
    sleep(5)
    println("After Sleep")

    system.terminate()

  }

  def sleep(s: Int): Unit ={
    Thread.sleep( s * 1000)
  }

  def startExperiment1(): Unit = {
    val system = ActorSystem("simakka")

    val simSystem = system.actorOf(Props[SimSystem], "SimSystem")

    simSystem ! "Test String Message"
    //    simSystem ! EndOfSimulation
    simSystem ! CreateEntity("Counter", "counter")
    sleep(1)
    simSystem ! CreateEntity("Client", "client")
    sleep(1)
    simSystem ! AddLinkN("client", "counter")
    sleep(1)
    simSystem ! InitEntity("client", Some("counter"))

    sleep(1)


//    simSystem ! SimStartTest

    println("before Sleep")
    sleep(5)
    println("After Sleep")

    system.terminate()

  }

  def main(args: Array[String]) {
    startExperiment1()
  }
}
