package simakka

/**
  *
  */
class SimInChannels  {


  var localTime = 0.0

  private var numEmptyChannels = 0

  def getNumEmptyChannels = numEmptyChannels


  /*Hold input channels as
  * key: id of SimEntity who is sending events to this instance
  * value: corresponding SimChannel*/
  val channelMap = scala.collection.mutable.Map[Int, SimInChannel]()


  def getLocalTime() = localTime

  /** true of any of the channels where empty **/
  def anyEmpty() = numEmptyChannels > 0


  def nonEmpty() = (channelMap.size > 0 )&& (numEmptyChannels == 0)
           


  /**
    * @param from : id of source SimEntity
    */
  def addLink(from: Int): Unit = {
    assert(!channelMap.contains(from))
    channelMap.put(from, new SimInChannel(from))
    numEmptyChannels += 1
  }


  def addEvent(nm: NullMessage): Unit = {
    
    
    
    assert(channelMap contains (nm.from))
    val channel = channelMap.get(nm.from).get
    if (channel.isEmpty()) numEmptyChannels -= 1
    channel += nm
    
  
  }


  def addEvent(ev: SimEv): Unit = {
    assert(channelMap contains (ev.from))
    val channel = channelMap.get(ev.from).get
    if (channel.isEmpty()) numEmptyChannels -= 1
    channel += ev
  }

  def probNextEvent(): (Int, SimEvent) = {
    val (minId, minEvent) = channelMap.mapValues(_.front()).minBy(_._2.time)
    (minId, minEvent)
  }

  def extractNextEvent(minId: Int): SimEvent = {
    val minChannel = channelMap.get(minId).get

    val ev = minChannel.getNextEvent()
    if (minChannel.isEmpty()) numEmptyChannels += 1
    ev
  }


  override def toString() = {
    val headTimes = for {i <- channelMap} yield (i._1, i._2.front, i._2.isEmpty())
    s"headTime = $localTime, queue = ${headTimes.mkString("\n")}"
  }

}

object SinInChannels {

  def testSimChannels: Unit = {
    val e1 = SimEv(2, 1, 2, 22, None)
    val e2 = SimEv(3, 1, 3, 33, None)
    val e3 = SimEv(4, 1, 3, 33, None)

    val channels = new SimInChannels()
    channels.addLink(2)
    channels.addLink(3)
    channels.addEvent(e1)
    channels.addEvent(e2)
    channels.addEvent(e3)

    println(channels)
    println(channels.probNextEvent())
    println(channels.extractNextEvent(2))

    println(channels.probNextEvent())

    println(channels.extractNextEvent(3))
  }

  def main(args: Array[String]): Unit = {
    println("Run ")
    testSimChannels
  }
}



