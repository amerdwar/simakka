package simakka.statistics

/**
  * 
  */


trait SimStat {
  
  val STAT_RATE = 0;
  val STAT_INTERVAL = 1;
  val STAT_TIME = 2;
  val STAT_USER = 3;

  var simTime: Double


  val stats = collection.mutable.Map[String, StatValue]()

  def getMeasure(name: String) = stats.getOrElse(name, 0)

  def updateMeasureTime(sv: StatValue, newValue: Double) = {
    sv.value=newValue;
    sv.accum = (simTime - sv.lastTime) * sv.value
    sv.lastTime = simTime
    
  }

  def updateMeasureRate(sv: StatValue) = {
    sv.accum += 1;
    sv.lastTime = simTime;
  }

  def updateMeasureInterval(sv: StatValue) = {
    sv.accum += simTime - sv.lastTime
    sv.lastTime = simTime;
  }

  def updateMeasureUser(sv: StatValue, value: Double) = {
    sv.accum += value
    sv.lastTime = simTime;
  }

  def addMeasure(stateType: Int, name: String): Unit = {
    stats.put(name, StatValue(stateType, 0, 0, 0))
  }

  def updateMeasure(name: String, value: Double) = {
    assert(stats.contains(name))
    val sv = stats.get(name).get

    sv.statType match {
      case STAT_TIME => updateMeasureTime(sv, value)
      case STAT_RATE => updateMeasureRate(sv)
      case STAT_INTERVAL => updateMeasureInterval(sv)
      case STAT_USER => updateMeasureUser(sv, value)
      case _ => println("not recognized")
    }
  }

  def getStat(name: String) = {
    stats.get(name)
  }

  def getStats = stats


}










