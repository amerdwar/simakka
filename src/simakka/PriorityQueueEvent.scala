package simakka

import scala.collection.mutable



object PriorityEventQueue {
type PEQueueType = collection.mutable.PriorityQueue[SimEv]

  def newInstance() = new PEQueueType()(new Ordering[SimEv] {
    override def compare(x: SimEv, y: SimEv): Int = Math.signum(y.time - x.time).toInt
  })

  def main(args: Array[String]) {
    def testPriorityEventQueue ={
      val e1 = SimEv(0, 1, 11, 22, None)
      val e2 = SimEv(2, 1, 11, 22, None)
      val e3 = SimEv(3, 1, 22, 22, None)

      val queue = newInstance
      queue.enqueue(e2)
      queue.enqueue(e1)
      queue.enqueue(e3)

      println(queue.dequeue())
      println(queue.dequeue())
      println(queue.dequeue())


    }

    testPriorityEventQueue
  }

}