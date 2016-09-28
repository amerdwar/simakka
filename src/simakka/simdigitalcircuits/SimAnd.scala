package simakka.simdigitalcircuits

class SimAnd(localName: String, localID: Int) extends SimCircuit(localName, localID){

  propogationDelay = 2.0

  override def calcOutput(ss: SimSignal): List[SimWire] = {
    val outWire = outWires.values.iterator.next()
    val result = ! inWires.values.exists( _ == false )
    if( result == outWire.level)
      return Nil
    outWire setLevel( result, simTime )
    List(outWire)
  }
}

class SimOr(localName: String, localID: Int) extends SimCircuit(localName, localID) {

  propogationDelay = 2.0
  override def calcOutput(ss: SimSignal): List[SimWire] = {
    val outWire = outWires.values.iterator.next()
    val result = inWires.values.forall( _ == true )
    if( result == outWire.level)
      return Nil
    outWire setLevel( result, simTime )
    List(outWire)
  }
}
class SimNot(localName: String, localID: Int) extends SimCircuit(localName, localID) {
  override def calcOutput(ss: SimSignal): List[SimWire] = {
    val outWire = outWires.values.iterator.next()
    val result = !ss.lvl
    outWire setLevel( result, simTime )
    List(outWire)
  }
}
