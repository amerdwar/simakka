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

trait ResponseTags{
  final val TAG_RES_INIT = 33
  final val TAG_REQ = 44
  final val TAG_RES = 55
  final val TAG_RES_END = 66
  final val size=4500;
}

class ResClient( localName: String,  localId: Int) extends SimEntity(localName , localId) with ResponseTags{

  var responder: String = "responder"
  var startTime = 0L
  var stopTime = 0L
  var sum=0L;
  var ttl=0L;
  var  resTime=0L;
  var co=0;
  private val rnd = new Random();
  val chararr = "abcdefghijklmnopqrstuvwxyz".toList
  
  
  
  def sampleData( lenght: Int): String ={
    var result = new StringBuilder()
    for( i <- 1 to lenght)
      result.append(chararr(rnd.nextInt(chararr.size)))
    return result.toString()
  } 
  
  
  
  override def handleEvent(ev: SimEv): Unit = {
  
    
  ev.tag match {
      case TAG_RES => {    
        co=co+1;
        stopTime = System.nanoTime();     
        ttl=stopTime-startTime;
        sum=sum+ttl;;
        if(co<=10000){
         startTime=System.nanoTime();
         schedule(0, TAG_REQ, ev.from,Some(sampleData(size)));
         flushOutEvents();
        }
        else{
          resTime =sum/co;
          resTime=resTime/1000;
          log.info(s"respose time=$resTime")
          
        }

      }

    }

  }


  override def initEntity(data: Option[String]): Unit = {
   var ss=data.get;
    startTime = System.nanoTime();
    schedule(0, TAG_REQ, ss, Some(sampleData(size)))
    flushOutEvents()  //To flush
    
  }

}

class Responder(localName: String, localId: Int) extends SimEntity(localName , localId) with ResponseTags{


  override def handleEvent(ev: SimEv): Unit = {
  
    ev.tag match {
      case TAG_REQ => {
    
        schedule(0, TAG_RES, ev.from,ev.data)

      }

    }

  }


  override def initEntity(data: Option[String]): Unit = {


  }
}

object ScenarioManagerRes extends SimConstants{


  def sleep(s: Int): Unit ={
    Thread.sleep( s * 1000)
  }

  def startExperiment2(): Unit = {
    val configFile = getClass.getClassLoader.
  getResource("application.conf").getFile
  val config = ConfigFactory.parseFile(new java.io.File(configFile))
  
    
    val system = ActorSystem("simakka",config)
    val simSystem = system.actorOf(Props[SimSystem], "SimSystem")
  
    simSystem ! "Test String Message"
  
    simSystem ! CreateRemoteEntity("Responder", "responder","akka.tcp://simakka@127.0.0.1:6666")
    sleep(1);
    simSystem ! CreateRemoteEntity("ResClient", "resclient","akka.tcp://simakka@127.0.0.1:2552")
    sleep(1);
    simSystem ! AddLinkN("resclient", "responder")
    simSystem ! AddLinkN("responder", "resclient")
    sleep(1);
    simSystem ! InitEntity("resclient", Some("responder"))

sleep(1);

  }

  def main(args: Array[String]) {
    startExperiment2()
  }

}
