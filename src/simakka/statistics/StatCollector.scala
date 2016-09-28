package simakka.statistics
import akka.actor.Actor;
import akka.actor.{Actor, ActorLogging, ActorRef, ActorSystem}
import akka.event.LoggingReceive

/**
 * 
 */
 class StatCollector extends Actor  with ActorLogging  {
    override def receive: Receive =  {



    case sm: StatValueM =>{
     log.info(sm.csvFormat)
    }
    }
}