import scala.collection.mutable

object Fifteen extends Input:
{
  val cavern = Array.tabulate(input.length, input.head.length)((y,x) => input(y)(x).asDigit)
  val entire = Array.tabulate(5*cavern.length,5*cavern.head.length)(
    (y,x) => Math.floorMod(cavern(y%cavern.length)(x%cavern.head.length)+y/cavern.length+x/cavern.head.length-1,9)+1
  )

  for(cave <- Seq(cavern, entire))
    val pq: mutable.PriorityQueue[((Int,Int),Int)] = mutable.PriorityQueue(((0,0),0))((l,r) => r._2-l._2)
    val vs = mutable.Set((0,0))
    while(pq.headOption.exists(_._1 != ((cave.length-1,cave.last.length-1)))) {
      val ((y1,x1), risk) = pq.dequeue
      for((y2,x2) <- Seq((y1+1,x1),(y1,x1+1),(y1-1,x1),(y1,x1-1)))
        if(!vs((y2,x2))
          && cave.indices.contains(y2)
          && cave(y2).indices.contains(x2)
        )
          pq.addOne(((y2,x2),risk+cave(y2)(x2)))
          vs.addOne((y2,x2))
    }
    pq.headOption.map(_._2).foreach(println)
}
