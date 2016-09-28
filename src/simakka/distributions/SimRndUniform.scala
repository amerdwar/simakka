package simakka.distributions

import scala.util.Random

/**
  * 
  */
class SimRndUniform(val name: String, lower: Double, upper: Double) extends SimRndContinuousGenerator {
  assert(upper > lower)
  val rnd = Random
  val rang = upper - lower

  var seed: Long = 454529944545L

  /**
    * Sample the random number generator.
    *
    * @return The sample
    */
  override def sample(): Double =
    lower + rang * rnd.nextDouble()

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
  override def getName(): String = this.name

  /**
    * Get the random number generator's seed.
    *
    * @return The generator's seed
    */
  override def getSeed() = this.seed
}
