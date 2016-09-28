package simakka

import java.util.concurrent.TimeUnit
import akka.actor.Actor.Receive
import akka.actor.{Actor, ActorLogging, ActorRef, ActorSystem}
import akka.event.LoggingReceive
import akka.pattern.ask
import akka.util.Timeout
import simakka.statistics._
import scala.concurrent.Await
import scala.concurrent.duration.{Duration, _}
import java.time.LocalDate
import scala.util.control.Exception
import java.lang.Exception
import javax.lang.model.`type`.NullType
import akka.actor.Kill
import akka.actor.ActorPath
import akka.actor.ActorRef




/**
 
  */

/**
  *
  * @param localName    Mandatory unique name for each SimEntity actor
  * @param localID Mandatory unique id for each of SimEntity actor
  *                //  * @param parent  ActorRef of parent actor (SimSystem)
  */
//class SimEntity(val name: String, val localID: Int, val parent: ActorRef)
abstract class SimEntity(val localName: String, val localID: Int)
  extends Actor with SimConstants with SimStat with ActorLogging with SimEntityLookup {
//var nullMessageNum=0;
//var eventMessageNum=0;
var df=0;
  val statMan = new StatMan()
statMan.addMeasure(statMan.STAT_RATE, "nullNum")
statMan.addMeasure(statMan.STAT_RATE, "eventNum")
var statCollector:ActorRef = null;
  /* Used to indicate the id of LocalEventQueue*/
//  final val LoopBackID = -1;

  /*Used as timeout of synchronous calls*/
  //implicit val timeout = Timeout(50) ; //TODO adjust it later


  /*Input channels*/
  val simInChannels = new SimInChannels()

  /*One separate ordered channel for local events */
  val localEventQueue = PriorityEventQueue.newInstance()

  val simOutChannels = new SimOutChannels;

  var simTime = 0.0;

  var lastEventTime = 0.0

  var lookahead = 0.5;




  /**
    * Send local events
    *
    * @param delay
    * @param tag
    * @param data
    */
  def scheduleLocal(delay: Double, tag: Int, data: Option[String]): Unit = {
    
    schedule(delay, tag, localID, localID, data)
  }


  /**
    * Send event from this SimEntity to other SimEntity by its id
    *
    * @param delay
    * @param tag
    * @param to
    * @param data
    */
  def schedule(delay: Double, tag: Int, to: Int, data: Option[String]): Unit = {
    schedule(delay, tag, localID, to, data)
  }

  /**
    * Send event from this SimEntity to other SimEntity by its name
    *
    * @param delay
    * @param tag
    * @param toS
    * @param data
    */
  def schedule(delay: Double, tag: Int, toS: String, data: Option[String]): Unit = {
//    val thatId = entitiesNames.get(toS)
    val thatId = getId(toS)

    if (thatId == None) {
      //TODO use getRef() methods here
      log.error("Could not find id for actor name:{}", toS)
      log.error(getLookInfo())
    } else {
      schedule(delay, tag, localID, thatId.get, data)
      
    }
  }

  /**
    * Send event between any other two SimEntities from this place, use SimEntity name for the source and destination
    *
    * @param delay
    * @param tag
    * @param fromS
    * @param toS
    * @param data
    */
  def schedule(delay: Double, tag: Int, fromS: String, toS: String, data: Option[String]): Unit = {

    val from = getId(fromS)
    val to = getId(toS)
    if (from == None || to == None) {
      //TODO use getRef() methods here
      log.error("Could not find id for on of actors : {}, {}", fromS, toS)
      log.error(getLookInfo())

    } else {
      schedule(delay, tag, from.get, to.get, data)
    }
  }

  /**
    * This method is called by all other variations,
    *
    * @param delay
    * @param tag
    * @param from
    * @param to
    * @param data
    */
  def schedule(delay: Double, tag: Int, from: Int, to: Int, data: Option[String]): Unit = {
    assert(delay >= 0 || contains(from) || contains(to))

    val nextTime = simTime + delay


    val ev = SimEv(nextTime, tag, from, to, data)
 
    /*Send the ev to the tmp destination SimEntity*/
    if (ev.to == localID) {

      localEventQueue.enqueue(ev)
          

    } else {
      
      simOutChannels += ev
      
    }

  }


  def handleEvent(ev: SimEv)

  def initEntity(data: Option[String])


  def updateStatistics(): Unit = {

  }


  def sendOutOneEvent(ev: SimEv): Unit = {
    val toRef = getRef(ev.to)
    if (toRef != None)
      toRef.get ! ev
  }

  private def sendOutEvents(time: Double, toId: Int, evs: Array[SimEv]): Unit = {
    if (getRef(toId) == None) return

    if (evs.nonEmpty)
      evs.foreach(sendOutOneEvent(_))
    else {
      var nullTime=0.0;
      nullTime = time+lookahead
      
   
      getRef(toId).get ! NullMessage(nullTime, localID, toId)
      
      //log.info("null message send time is "+nullTime)
    }
  }


  def canAdvance() = {
    var d=simInChannels.nonEmpty()
          d
          
      var d2=simInChannels.channelMap.size==0 && localEventQueue.size>0
      d || d2
  }
    
  


  def getNextEvent(): SimEvent = {
    
    var result:SimEvent =null;
    if(simInChannels.channelMap.size==0){//so deque local
      try{
      result= localEventQueue.dequeue()
statMan.updateMeasure("eventNum", 1)
      }catch{
        
        case ex:Exception => log.info("the exceptino is here"+this.localName)
      }
      
    }else{//dequeue local or inchannels
    val (minId, minEvent) = simInChannels.probNextEvent()
  
    if (localEventQueue.nonEmpty
      && localEventQueue.head.time < minEvent.time) {
     result= localEventQueue.dequeue()
     statMan.updateMeasure("eventNum", 1)
    } else {
     result= simInChannels.extractNextEvent(minId)
    }
    }
    result
  }

  def tick(): Unit = {



    var timeChanged = false

   
    while (canAdvance) {
          
      val simEvent = getNextEvent()
      if (simEvent.time > simTime) {
        
        timeChanged = true
        lastEventTime = simTime
        simTime = simEvent.time
        
        updateStatistics()
      }
      
     if(simEvent.isInstanceOf[SimEv]){
       
       
       handleEvent(simEvent.asInstanceOf[SimEv])
       
      }
    

    }

      
      flushOutEvents()
    

 
  }


  def flushOutEvents() ={
    
    
    
    val outEvents = simOutChannels.getAllToflush()
    //    outEvents.foreach( entry => sendOutEvent(entry._1, entry._2))
    outEvents.map(kv => sendOutEvents(simTime, kv._1, kv._2))

  }
  def endSimulation {
self ! EndOfSimulation
  }
  override def receive: Receive =  {


    case POCK =>
      
      sender ! PockBack(localName, localID)

    case SimStart =>{
      tick()
    }

    case nm: NullMessage =>
   statMan.updateMeasure("nullNum", 1)
      simInChannels.addEvent(nm)
      
      tick()
    
    case ev: SimEv =>{
      
    statMan.updateMeasure("eventNum", 1)
      
     if(ev.from==localID){
       
       localEventQueue.enqueue(ev)
    //      log.info("add event"+localEventQueue.size)
     }
       else
       {
         simInChannels.addEvent(ev)
    
       }
          tick()
    }

    case ie: InitEntity =>
      log.info("recive init entity");
      initEntity(ie.data)
    
    
    case alf: AddLinkFrom =>
      
      assert(alf.to == localID);
      simInChannels.addLink(alf.from)

    case alt: AddLinkTo =>
      
      assert(alt.from == localID);
      simOutChannels.addLink(alt.to)

    case ar: AddRef =>
      
      addRef(ar.name, ar.id, ar.actorRef)
      

    case SetLookahead(delay, value) =>
      scheduleLocal(delay, TAG_SET_LOOKAHEAD, Some(value.toString))

    case EndOfSimulation =>{
     df+=1;
    println(this.localName+ " "+ df)
      //to do do not send endofsimulation message to the souce of that message to avid cyclying
      statMan.stats.foreach{a => 
       
        
        var pp:StatValue =  a._2.asInstanceOf[StatValue];
         statCollector ! pp.toStatValueM(this.localName,a._1)     
   
      
      
      }
      var dm  = List[ActorRef]()
      simOutChannels.outMap.foreach {kv =>{ var pp=getRef(kv._1) ;
       
    if(!context.sender().equals(pp.get)){
         
     dm::=pp.get; 
     
        pp.get ! EndOfSimulation
       println(context.sender().path.toString()+"  "+pp.get.path.toString() + "dm size"+dm.size) 
      }
     }
     }

      
      simInChannels.channelMap.foreach {kv =>{ var pp=getRef(kv._1) ;

      //
     if(!context.sender().equals(pp.get) && !dm.exists { x =>x.equals(pp.get) }){
     pp.get ! EndOfSimulation
     println(context.sender().path.toString()+"  "+pp.get.path.toString()+"   2 dm size "+dm.size)
     }
     }
     }
     
      self ! akka.actor.PoisonPill
   //self ! Kill 
      
      
    }
  
      
    case "Test" => log.info("Test")
    case ss:SetStatCollector=>{
      statCollector=ss.ref;
      
    }
    case _ => log.info("Unknown Message")
  }


}
    class StatMan extends SimStat {   var simTime = 0.0 }