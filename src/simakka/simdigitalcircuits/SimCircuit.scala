package simakka.simdigitalcircuits

import java.util.concurrent.atomic.AtomicInteger

import akka.event.{LoggingAdapter, LoggingReceive}
import simakka.{SetLookahead, _}

import scala.collection.mutable



case class SimProb( val la: Option[LoggingAdapter]) {
  def log(msg: String): Unit = {
    if (la == None)
      println(msg)
    else
      la.get.info(msg)
  }
}

object SimWire {
  val ID = new AtomicInteger
  def newID = ID.incrementAndGet()

  def stripName(name: String) = name.replace("+", "").replace("*", "").replace("#", "")
  def of(s: String): SimWire = of(s, None)

  // outwire*#+ >> c1 c2
  // name/*probe/+level
  def of(wire: String, log: Option[LoggingAdapter]): SimWire = {
    val parts = wire.split(">>").map(_.trim)
    val p1 = parts(0)
    val name = stripName(p1)
    val withProbe = p1.contains("*")
    val plevel = p1.contains("+")
    val result = new SimWire(name)
    if (parts.size == 2) {
      parts(1).split("\\s+").foreach(result.attachTo(_))
    }
    if (plevel) result.setLevel(plevel, 0.0)
    if (withProbe) {
      result.isLogged = true
      result.setSimProb(SimProb(log))
    }
    result
  }


}

class SimWire(val name: String) {
  val id = SimWire.newID

  var level = false;
  var propagationDelay = 0.0
  var time = 0.0
  val to = mutable.Set[String]()
  val from = mutable.Set[String]()

  def setProbDelay(d: Double) = {
    propagationDelay = d
    this
  }

  def toSimSignal(tm: Double) = SimSignal(name, level, tm)

  var simProb: SimProb = null
  var isLogged = false

  def setSimProb(sb: SimProb) = {
    isLogged = sb != null
    simProb = sb
    this
  }

  def attachTo(nms: String*): Unit = {
    for (nm <- nms) {
      to += nm
    }
  }
  def attachFrom(nms: String*): Unit = {
    for (nm <- nms) {
      from += nm
    }
  }

  def setLevel(lvl: Boolean): Boolean = setLevel(lvl, time)

  def setLevel(lvl: Boolean, tm: Double): Boolean = {
    if (lvl == level)
      false
    else {
      level = lvl
      time = tm
      if (isLogged) {
        simProb.log(s"wire:$name, time:$tm, level:$level")
      }
      //notify the attached gates with the changes
      true
    }
  }

  def toSend(): String = {
    // outwire1* >> c1 c2
    val ib = if(isLogged) "*" else ""
    val hl = if(level) "+" else ""
    val att = if (to.size > 0)
      ">> " + to.mkString(" ")
    else ""

    s"$name$ib$hl $att"
  }

  override def toString(): String = {
    s"Wire($name), level=$level, isLogged=$isLogged, attachedCircuits=${to.mkString(",")}"
  }

  //  def getAttachedCircuits() = attachedCircuits
}

object SimSignal {
  //sname,lbl,tm
  def of(str: String): SimSignal = {
    str.split(",") match {
      case Array(nm, lv, tmm) =>
        SimSignal(nm, lv.toBoolean, tmm.toDouble)
    }
  }
}

case class SimSignal(name: String, lvl: Boolean, tm: Double) {


  override def toString(): String = {
    s"$name,$lvl,$tm"
  }
}

abstract class SimCircuit(localName: String, localID: Int)
  extends SimEntity(localName, localID) {

  final val TAG_SIGNAL = 77

  override def handleEvent(ev: SimEv): Unit = {
    val outWires = ev.tag match {
      case TAG_SIGNAL =>
      //        calcOutput(SimSignal.of(ev.data.get))
    }

    //log the probs


    //sendout signals

  }

  def calcOutput(ss: SimSignal): List[SimWire]

  var propogationDelay: Double = 1 // TODO change it to 1e-8
  var inWires = collection.mutable.HashMap[String, Boolean]()
  var outWires = collection.mutable.HashMap[String, SimWire]()

  // inwire1+, inwire2+, inwiren =>
  // outwire1* >> c1 c2, outwire2*+ >> c2 c3 => propagation
  override def initEntity(data: Option[String]): Unit = {
    val sec = data.get.split("=>").map(_.trim)
    if (sec.size == 3) {
      propogationDelay = sec(2).toDouble
    }


    //Setup inWires
    sec(0).split(",").map(_.trim).foreach { w =>
      val nm = w.replace("+", "")
      val lvl = w.contains("+")
      inWires.put(nm, lvl)
    }

    //Setup outWires
    sec(1).split(",").map(_.trim).foreach { s =>
      val wr = SimWire.of(s, Some(log))
      outWires.put(wr.name, wr)
    }

  }

  def sendOutChanges(simWire: SimWire, tm: Double): Unit = {
    val simSignal = simWire.toSimSignal(tm)
    simWire.to.foreach {
      cnm =>
        getRef(cnm).get ! simSignal
    }
  }

  def processDigital(ss: SimSignal): Unit = {

    val outTime = Math.max(ss.tm + propogationDelay, simTime)

    simTime = Math.max(simTime, ss.tm)

    //Get the input
    val oldInValue = inWires.get(ss.name).get
    //Calc output if input is changed
    if (oldInValue != ss.lvl) {
      inWires.put(ss.name, ss.lvl)
      val changedWires = calcOutput(ss)
      changedWires.foreach(cw => sendOutChanges(cw, outTime))
    }
  }

  override def receive: Receive =
    LoggingReceive {

      case ss: SimSignal => processDigital(ss)

      case POCK =>
        log.debug(s"$localName: got POCK from system")
        sender ! PockBack(localName, localID)

      case SimStart => {
        tick()
      }

      case nm: NullMessage =>
        log.debug(s"$localName: $nm")
        simInChannels.addEvent(nm)
        tick()

      case ev: SimEv =>
        simInChannels.addEvent(ev)
        tick()


      case ie: InitEntity =>
        log.debug(s"$localName : $ie")
        initEntity(ie.data)


      //    case ev: SimEv =>
      case alf: AddLinkFrom =>
        log.debug(s"$localName : $alf")
        assert(alf.to == localID);
        simInChannels.addLink(alf.from)

      case alt: AddLinkTo =>
        log.debug(s"$localName : $alt")
        assert(alt.from == localID);
        simOutChannels.addLink(alt.to)

      case ar: AddRef =>
        log.debug(s"$localName : addRef $ar")
        addRef(ar.name, ar.id, ar.actorRef)
        log.debug(s"$localName: lookup info=$getLookInfo()")

      case SetLookahead(delay, value) =>
        scheduleLocal(delay, TAG_SET_LOOKAHEAD, Some(value.toString))

      case "Test" => log.info("Test")

      case _ => log.info("Unknown Message")
    }


}


object TestSimWire {
  def main(args: Array[String]) {
    val s = "nm+ >> c1*"
    val wire = SimWire.of(s)
    println(wire.toString())

  }
}
