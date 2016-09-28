package simakka

/**
  * 
  */

sealed trait SimEvent {
  val time: Double
}

case class SimEv(time: Double, tag: Int, from: Int, to: Int, data: Option[String]) extends SimEvent

case class NullMessage(time: Double, from: Int, to: Int) extends SimEvent

final object SimEvNone extends SimEvent {
  override val time: Double = Double.MaxValue
}



