package simakka.distributions

/**
  * 
  */
trait SimRndGenerator {

  val name: String
  var seed: Long

  /**
    * Set the random number generator's seed.
    *
    * @param seed The generator's seed
    */
  def setSeed(seed: Long)


  /**
    * Get the random number generator's seed.
    *
    * @return The generator's seed
    */
  def getSeed(): Long

  /**
    * Get the random number generator's name.
    *
    * @return The generator's name
    */
  def getName(): String
}
