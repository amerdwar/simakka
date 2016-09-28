package simakka.statistics

/**
  * Basic unit to store one measure
  *
  * @param statType
  * @param value
  * @param accum
  * @param lastTime
  */
class StatValue(val statType: Int, var value: Double, var accum: Double,
                var lastTime: Double) {

                  def toStatValueM(entityName: String, statName: String) =
                    StatValueM(entityName, statName,statType, value, accum, lastTime)

                  override def toString = {
                    s"StateValue(statTyple:$statType, value:$value, accum:$accum, lastTime:$lastTime)"
                  }
                }

/**
  * For providing apply method and future static vars and functions
  */
object StatValue {

  def apply(statTyple: Int, value: Double, accum: Double, lastTime: Double) =
    new StatValue(statTyple, value, accum, lastTime)

  def testStatValue(): Unit ={
    /** dummy class used instead of SimEntity to test statValue */
    class TestStat extends SimStat {   var simTime = 0.0 }

    val ts = new TestStat()

    ts.addMeasure(ts.STAT_TIME, "arrivals")
    ts.simTime = 1;
    ts.updateMeasure("arrivals", 3)
    println(ts.getStat("arrivals"))
    ts.simTime = 10
    ts.updateMeasure("arrivals", 2)
    println(ts.getStat("arrivals"))
    val tsm = ts.getStat("arrivals").get.toStatValueM("entity1", "stat1")

    println(tsm)
    println(StatValueM.header)
    println(tsm.csvFormat)
  }

  def main(args: Array[String]) {
    testStatValue()
  }
}