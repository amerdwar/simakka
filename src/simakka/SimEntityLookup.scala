package simakka

import akka.actor.ActorRef

/**
  *
  */
trait SimEntityLookup {
  final private val entities = scala.collection.mutable.HashMap[Int, ActorRef]()
  final private val entitiesNames = scala.collection.mutable.HashMap[String, Int]()

  def newSimEv(time: Double, tag: Int, fromS: String, toS: String, data: Option[String]) = {
    val from = getId(fromS).get
    val to = getId(toS).get
    SimEv(time, tag, from, to, data)
  }

  def getRef(idd: Int): Option[ActorRef] = {
    assert(entities.size == entitiesNames.size)
    entities.get(idd)
  }

  def getId(nm: String) = {
    assert(entities.size == entitiesNames.size)
    entitiesNames.get(nm)
  }

  def getRef(name: String): Option[ActorRef] = {
    assert(entities.size == entitiesNames.size)

    val lid = entitiesNames.get(name)
    if (lid == None) None
    else entities.get(lid.get)
  }

  def addRef(nm: String, idd: Int, actorRef: ActorRef): Unit = {
    println(s"addref nm:$nm , idd:$idd, actorRef:$actorRef")

    if (entitiesNames.contains(nm)) {
      val storedId = entitiesNames.get(nm).get
      assert(storedId == idd)
    }

    val bid = entities.put(idd, actorRef)
    val bnm = entitiesNames.put(nm, idd)

    assert(entities.size == entitiesNames.size) //TODO delete it later

  }

  def contains(idd: Int) = entities.contains(idd)

  def contains(nm: String) = entitiesNames.contains(nm)

  def getLookInfo(): String = {
    assert(entities.size == entitiesNames.size)

    val names = entitiesNames.keySet.mkString(", ")
    val ids = entities.keySet.mkString(", ")
    s"names: ${entitiesNames.size} = $names, entites: ${entities.size} = $ids" //TODO delete it later
  }
}

class TSEL

object TestSimEntityLookup {
  def main(args: Array[String]) {
    println("start")
    val a = new TSEL with SimEntityLookup
    val b = new TSEL

    a.addRef("rf1", 1, null)
    a.addRef("rf2", 2, null)
    a.addRef("rf3", 3, null)

    println(a.getLookInfo())

  }
}
