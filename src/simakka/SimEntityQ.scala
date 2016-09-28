package simakka

import simakka.distributions._
/**
  *
  */
class SimEntityQ(override val localName: String, val id: Int) extends SimEntity(localName, id){

  addMeasure(STAT_INTERVAL, "stat_waiting_times")
  addMeasure(STAT_RATE, "stat_number_events")
  addMeasure(STAT_TIME, "stat_server_utilication")
  addMeasure(STAT_TIME, "stat_queue_lenght")

//  val s =
  SimRndExp("rnd_service_time", 5)
  SimRndExp("rnd_arrival_time", 6)

  override def handleEvent(ev: SimEv): Unit = {

  }

  override def initEntity(data: Option[String]): Unit = {

  }
}
