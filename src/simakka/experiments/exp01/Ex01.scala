package simakka.experiments.exp01

import java.util.Random
import akka.actor.{ActorSystem, Props,AddressFromURIString}
import simakka.{AddLinkN, CreateEntity, InitEntity, _}
import akka.stream.stage.Context
import com.typesafe.config.ConfigFactory
import simakka.statistics.SimStat
import akka.remote._
/**
  * 
  */

trait CounterTags{
  final val TAG_COUNTER_INIT = 33
  final val TAG_COUNTER_COUNT = 44
  final val TAG_COUNTER_END = 55
}

class Client( localName: String,  localId: Int) extends SimEntity(localName , localId) with CounterTags{

  var counterName: String = "counter"



  override def handleEvent(ev: SimEv): Unit = {
    println("")

  }

  private val rnd = new Random();
  val chararr = "abcdefghijklmnopqrstuvwxyz".toList
  def sampleData( lenght: Int): String ={
    var result = new StringBuilder()
    for( i <- 1 to lenght)
      result.append(chararr(rnd.nextInt(chararr.size)))
    return result.toString()

  }
  override def initEntity(data: Option[String]): Unit = {
    log.info(s"$localName: init with data = $data")
    counterName = data.get


    schedule(0, TAG_COUNTER_INIT, counterName, Some(sampleData(0)))

    flushOutEvents()  //To flush
val aa=sampleData(5000);
    for (i <- 1 to 10000) {
      schedule(0, TAG_COUNTER_COUNT, counterName, Some(aa))
        flushOutEvents()
//      tick()
    }
  
    schedule(0, TAG_COUNTER_END, counterName, None)
    flushOutEvents()

    log.info(s"$localName, End of App ")
  }

}

class Counter(localName: String, localId: Int) extends SimEntity(localName , localId) with CounterTags{

  var counter = 0
  var startTime = 0L
  var stopTime = 0L

  override def handleEvent(ev: SimEv): Unit = {
    log.debug(s"$localName: received $ev")
    ev.tag match {
      case TAG_COUNTER_INIT => {
        startTime = System.nanoTime()
        log.debug(s"$localName: startTime: $startTime")
      }

      case TAG_COUNTER_COUNT => {
        counter += 1
//        log.debug(s"$localName: counterValue: $counter")
      }

      case TAG_COUNTER_END => {
        println(s"counter = $counter")
        stopTime = System.nanoTime();
        log.debug(s"$localName: stopTime: $stopTime")

        val duration = stopTime - startTime
        val thpt = (1e9 * counter + 0.0) / duration
        log.info(s"Throughput = $thpt")
      }
    }

  }


  override def initEntity(data: Option[String]): Unit = {


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
  
    
    val system = ActorSystem("simakka",config)
    val simSystem = system.actorOf(Props[SimSystem], "SimSystem")
//    println( simSystem.path.toStringWithAddress(AddressFromURIString("akka.tcp://simakka@127.0.0.1:6666")));

    
    simSystem ! "Test String Message"
    //    simSystem ! EndOfSimulation
    simSystem ! CreateRemoteEntity("Counter", "counter","akka.tcp://simakka@127.0.0.1:6666")
    
    simSystem ! CreateEntity("Client", "client")
    
    simSystem ! AddLinkN("client", "counter")
    
    simSystem ! InitEntity("client", Some("counter"))


   


//    simSystem ! SimStartTest



  
    
    

  }

  def main(args: Array[String]) {
    startExperiment1()
  }

}
