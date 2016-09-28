package simakka.distributions

import scala.util.Random

/**
  * 
  */
class SimRndExp(val name: String, val mean: Double) extends SimRndContinuousGenerator {

  val rnd = Random

  /**
    * Sample the random number generator.
    *
    * @return The sample
    */
  override def sample(): Double = {
    -mean * Math.log(rnd.nextDouble())
  }

  /**
    * Set the random number generator's seed.
    *
    * @param seed The generator's seed
    */
  override def setSeed(seed: Long): Unit = {
    this.seed = seed
    rnd.setSeed(seed)
  }

  /**
    * Get the random number generator's name.
    *
    * @return The generator's name
    */
  override def getName(): String = name

  /**
    * Get the random number generator's seed.
    *
    * @return The generator's seed
    */
  override def getSeed() = seed

  override var seed: Long = 445454545L
}

object SimRndExp{
  def apply(name: String, mean: Double): SimRndExp = new SimRndExp( name, mean)

  def main(args: Array[String]) {
    new SimRndExp("suhel", 5.6)
  }
}