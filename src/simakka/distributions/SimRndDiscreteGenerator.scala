package simakka.distributions

/**
  *
  */
trait SimRndDiscreteGenerator extends SimRndGenerator {
  /**
    * Sample the random number generator.
    *
    * @return The sample
    */
  def sample(): Long
}
