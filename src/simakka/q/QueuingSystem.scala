package simakka.q;
import simakka._
import simakka.distributions._
import akka.remote.transport.AssociationHandle.HandleEvent
import akka.actor.Props
import scala.xml.Null
import scala.collection.immutable.Nil
import com.typesafe.config.ConfigFactory
import java.lang.Double
import akka.cluster.ddata.Replicator.UpdateSuccess
import simakka.statistics._

trait SourceTags{
  final val TAG_START_SOURCE= 33
  final val TAG_CUSTOMER_ARRIVE= 44
  final val TAG_CUSTOMER_DEPA= 55
  final val TAG_END_SIMULATION= 66

  
}

class Source( localName: String,  localId: Int) extends SimEntity(localName , localId) with SourceTags{

  var MIN_ARRIVAL_TIME=0.0;
  var MAX_ARRIVAL_TIME=0.0;
  var a=1;
  var randArival:SimRndUniform=null;
  var serverName="";
 override def handleEvent(ev: SimEv): Unit = {
   
 ev.tag match {
   case TAG_START_SOURCE => {
   //simTime=simTime+s;
     var s=0.0;
        
    s = randArival.sample()
    schedule(s,TAG_CUSTOMER_ARRIVE, serverName,null)//send customer to client 
      
    
    if( a < 10000){
    a+=1; 
    
    scheduleLocal(s+0.1,TAG_START_SOURCE, null)//send customer to client 
    if(a==10000){
        println("***********source statistics***************")
           println
        //  println("null message num " +nullMessageNum )
        //  println("event message num " +eventMessageNum )
    println
    println
    
      }
        
      
    }

   }
  
 }
   
 
 }
  
 override def initEntity(data: Option[String]): Unit = {
   //to do extraxt data from data array
   
   serverName=data.get;
   MIN_ARRIVAL_TIME=8.0;
   MAX_ARRIVAL_TIME=12.0;
   randArival=  new SimRndUniform("arrival",MIN_ARRIVAL_TIME,MAX_ARRIVAL_TIME);
   
   scheduleLocal(0.3,TAG_START_SOURCE, null)
   tick()
   log.info("init is source");

   

 }
    
  
}



class Server( localName: String,  localId: Int) extends SimEntity(localName , localId) with SourceTags{
  var NUM_CUSTOMERS_SERVED=0.0;
  var MIN_SERVICE_TIME=0.0;
  var MAX_SERVICE_TIME=0.0;
  var NUM_LIMITE=20000;
  var IDELE=0;
  var BUSY=1;
  var randService:simakka.distributions.SimRndUniform=null;
  var i=0;
  var startTime=0.0;
  var endTime=0.0;
  //define variable of statistics
  var numDelayedCustomers=0.0;
  var serverStatus=IDELE;
  var numInQ=0.0;
  var totalDelays=0.0;
  var delay=0.0;
  var timeLastEvent=0.0;
  var areaNumInQ = 0.0;
  var areaServerStatus=0.0;
  
  
  
      
  
  
  
  val queue = scala.collection.mutable.Queue[Double]()
  
 override def initEntity(data: Option[String]): Unit = {
   //to do extraxt data from data array
       NUM_CUSTOMERS_SERVED=10000;
       MIN_SERVICE_TIME = 1.0
       MAX_SERVICE_TIME = 2.0
       randService=  new SimRndUniform("service",MIN_SERVICE_TIME,MAX_SERVICE_TIME);
       log.info("init server done");
       
       
     
       statMan.addMeasure(statMan.STAT_USER , "totalDelays")

       statMan.addMeasure(statMan.STAT_USER , "averageDelays")
       statMan.addMeasure(statMan.STAT_USER , "serverUtil")
     statMan.addMeasure(statMan.STAT_USER , "averageQlen")  
       
       
   
 }
  override def handleEvent(ev: SimEv): Unit = {
    
       
 ev.tag match {
   
   case TAG_CUSTOMER_ARRIVE => {
     if(numDelayedCustomers==0){
       startTime=System.nanoTime();
       
     }
 //  println("arrive at "+ev.time)
   updateStat;  

     //increase the number in q
     if(serverStatus==BUSY){
      numInQ+=1;
      
       if(numInQ>NUM_LIMITE){
         log.info("run out queue")
         
       }else{
         //add to q
         queue.enqueue(ev.time)
        
         
       }
      
     }else{//server is idle
       delay=0; //there is no delay because the customer has been served im
       totalDelays+=delay;
       statMan.updateMeasure("totalDelays",0);
        
       numDelayedCustomers+=1;
    
       if(numDelayedCustomers==NUM_CUSTOMERS_SERVED){
          
           endSimAndPrint
           
          
       
           println("***************** end simulation   **************");
       
         }
        serverStatus=BUSY;
       var ss=randService.sample();
       i=i+1;
       scheduleLocal(ss, TAG_CUSTOMER_DEPA, null)
       
       
       
     }
   }
   
   case TAG_CUSTOMER_DEPA =>{
  //println("Dep at "+ev.time+" num cust ser "+numDelayedCustomers)   
  updateStat

     if(numInQ==0){
       
       
         serverStatus=IDELE;
       
     }else{
       numInQ-=1;

       var de=queue.dequeue();
       delay=simTime-de
       totalDelays+=delay;
       
       statMan.updateMeasure("totalDelays",delay);
       
       var ss=randService.sample();
       
       numDelayedCustomers+=1;
  
       if(numDelayedCustomers== NUM_CUSTOMERS_SERVED){
         
         endSimAndPrint
         
         println
         println("****************** Simulation End  ********************")
       
         }
       scheduleLocal(ss, TAG_CUSTOMER_DEPA, null)
           
       

     }
    }
   }
 
 }
  
  def updateStat():Unit = {
   var timeSinceLastE = 0.0;
   timeSinceLastE = simTime-timeLastEvent;//here we calculate the time slide 
   timeLastEvent =  simTime;
   areaNumInQ += numInQ*timeSinceLastE;
   
   areaServerStatus += serverStatus*timeSinceLastE;
  
  }
  def endSimAndPrint()={
    println
    println("***************server Statistics******************")
    println
    
   
    println("sim time is = "+simTime)
     /*
    println("toltal delays = "+totalDelays)
    
    println
    println("num delayed customers = "+numDelayedCustomers)
    println("average delay :"+totalDelays/numDelayedCustomers);
    println("average q len :"+areaNumInQ/simTime);
    println("average utlization :"+areaServerStatus/simTime);
    println
    */
      
     statMan.updateMeasure("averageDelays", totalDelays/numDelayedCustomers)
     statMan.updateMeasure("averageQlen", areaNumInQ/simTime)
     statMan.updateMeasure("serverUtil", areaServerStatus/simTime)
  /*  
    println
    println("null message num " +statMan.getMeasure("nullNum") )
    println("event message num " +statMan.getMeasure("eventNum") )
    
    println
    */
     
    endTime=System.nanoTime();
       
    endTime=(endTime-startTime)/1000000;
    println
    println("the time is "+endTime)
    
    
    endSimulation

  }
  

}


object ScenarioManager extends SimConstants{


  def sleep(s: Int): Unit ={
    Thread.sleep( s * 1000)
  }

  def startExperiment1(): Unit = {
    val configFile = getClass.getClassLoader.
  getResource("application.conf").getFile
  val config = ConfigFactory.parseFile(new java.io.File(configFile))
  
    
    val system = akka.actor.ActorSystem("simakka",config)
    val simSystem = system.actorOf(Props[SimSystem], "SimSystem")
    
    val statCollector = system.actorOf(Props[StatCollector], "StatCollector")
//    println( simSystem.path.toStringWithAddress(AddressFromURIString("akka.tcp://simakka@127.0.0.1:6666")));

    simSystem ! SetStatCollector(statCollector)
    simSystem ! "Test String Message"
    //    simSystem ! EndOfSimulation
  //  simSystem ! CreateRemoteEntity("Counter", "counter","akka.tcp://simakka@127.0.0.1:6666")
    
   // simSystem ! CreateEntity("Client", "client")
    
   // simSystem ! AddLinkN("client", "counter")
    
    //simSystem ! InitEntity("client", Some("counter"))
    simSystem ! CreateEntity("Source","source");
   // simSystem ! CreateRemoteEntity("Source", "source","akka.tcp://simakka@127.0.0.1:2552")
    sleep(1);
    simSystem ! CreateEntity("Server","server");
//    simSystem ! CreateRemoteEntity("Server", "Server","akka.tcp://simakka@127.0.0.1:2552")
    sleep(1);
    
    simSystem ! AddLinkN("source", "server")
    simSystem ! AddLinkN("server", "source")
    
    sleep(1);
    simSystem ! InitEntity("server",null )
    sleep(1);
    simSystem ! InitEntity("source", Some("server"))
    
    sleep(1);
//    simSystem ! SimStartTest
  }

  def main(args: Array[String]) {
    startExperiment1()
  }

}





