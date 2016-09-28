package simakka.distributions

/**
  * 
  */
trait SimRndContinuousGenerator extends SimRndGenerator {
  /**
    * Sample the random number generator.
    *
    * @return The sample
    */
  def sample(): Double
}
