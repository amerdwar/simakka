package simakka

import simakka.PriorityEventQueue.PEQueueType

/**
  *
  */



class SimOutChannels {

  val outMap = scala.collection.mutable.Map[Int, PEQueueType]()


  var numEmptyChannels = 0

  def nonEmpty() = outMap.values.exists( _.nonEmpty)

  def addLink(to: Int): Unit = {
    if (!outMap.contains(to)) {
      outMap.put(to, PriorityEventQueue.newInstance())
      numEmptyChannels += 1
    }
  }


  /**
    * Get all elements in queue, save them ordered in Array
 *
    * @param oq
    * @return
    */
  def flushArrayEvents(oq: PEQueueType): Array[SimEv] = {
    val result = Array.fill(oq.size)(oq.dequeue)
    result
  }

  def getAllToflush() = {
    outMap.mapValues( flushArrayEvents( _ ) )
  }

  def +=(simEv: SimEv): Unit = {

    addEvent(simEv)
  }

  def addEvent(simEv: SimEv): Unit = {
    assert(outMap.contains(simEv.to))
    outMap.get(simEv.to).get.enqueue(simEv)
  }

}


















