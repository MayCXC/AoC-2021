object Six extends Input:
{
  val initial = input.head.split(",").map(_.toInt).groupMapReduce(identity)(_ => BigInt("1"))(_+_)

  def simulate(state: Map[Int,BigInt]) = state.flatMap[(Int,BigInt)](
    (k,v) =>
      if(k>0) List((k-1,v))
      else List((6,v),(8,v))
  ).groupMapReduce(_._1)(_._2)(_+_)

  println(LazyList.iterate(initial)(simulate)(80).values.sum)
  println(LazyList.iterate(initial)(simulate)(256).values.sum)
}
