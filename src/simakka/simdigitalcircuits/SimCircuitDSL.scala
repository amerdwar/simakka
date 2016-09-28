package simakka.simdigitalcircuits

import java.util.concurrent.atomic.AtomicInteger

class SimCircuitDSL {

}

trait DSLCircuitTrait {
  val circuitType: String
  val name: String

  val wires = collection.mutable.Set[SimWire]()
  val circuits = collection.mutable.Set[DSLCircuit]()
  var in: Seq[String] = collection.mutable.ArrayBuffer[String]()
//  var out: Seq[String] = collection.mutable.ArrayBuffer[String]()
  var out = ""

  def wire(names: String*) = {
    val result = names.map(SimWire.of(_))
    result.foreach(wires += _)
  }

  def and(name: String)(in: String*)(out: String) = of("SimAnd")(name)(in)(out)

  def and(in: String*)(out: String): DSLCircuit =
    of("SimAnd")(s"AND_${DSLCircuit.newID}")(in)(out)

  def or(name: String)(in: String*)(out: String) = of("SimOr")(name)(in)(out)

  def or(in: String*)(out: String): DSLCircuit =
    of("SimOr")(s"OR_${DSLCircuit.newID}")(in)(out)

//  def not(name: String)(in: String)(out: String) = of("SimNot")(name)(in::Nil)(out)

  def not(in: String)(out: String): DSLCircuit =
    of("SimNot")(s"NOT_${DSLCircuit.newID}")(in::Nil)(out)

  def printAllCircuits(): Unit = {
    println("------Wires--------")
    wires.foreach(println)
    println("---------Circuits----------")
    circuits.foreach(println)
  }

  override def toString(): String = {
    s"$circuitType=>$name=>${in.mkString(",")}=>$out}"
  }

  def buildCircuit() {}



  def of(circuitType: String)(name: String)
        (in: Seq[String])(out: String): DSLCircuit = {
    val result = new DSLCircuit(circuitType, name,
      DSLCircuit.newID)(in)(out)


//    println(result.circuits.mkString("\n---||----\n"))
    val strippedNames = in map SimWire.stripName
    result.wires.filter(strippedNames contains _.name).foreach(_.attachTo(result.name))
    result
  }

}

object DSLCircuit {
  val atomicID = new AtomicInteger

  def newID = atomicID.incrementAndGet()

}

object DSLWire {

  def test = {
    val w = SimWire.of("w1*>>d f")
    w.setLevel(true)
    w.setLevel(false)
    w.setLevel(true)



    println(w.toString())
    println(w.toSend())
    println(SimWire.of(w.toSend()).toSend())
  }

  def main(args: Array[String]) {

    val mc = new MyCircuit("mycircuit","mytype")
    mc.buildCircuit()

    mc.printAllCircuits()
    println(mc.toString())

  }
}


class DSLCircuit(val circuitType: String, val name: String, val circuitID: Int)
                ( ffin: Seq[String])
                ( out: String) extends DSLCircuitTrait{

  def toSend(): Unit ={

  }


}


class MyCircuit(override val name: String, val circuitType: String )
  extends DSLCircuitTrait {

  override def buildCircuit() {

    wire("A*", "B*", "C*", "D", "E", "F*", "G*")

    or("A", "B")("F")

    not( "A")("D")

    and("C", "D")("E")

    or("B", "E")("G")
  }

  def simulateCircuit(): Unit ={





  }



}
