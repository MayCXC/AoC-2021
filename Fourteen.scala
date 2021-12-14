object Fourteen extends Input:
{
    val (template, rules) = (input.head, input.tail.tail.map{case s"$l -> $r" => l->r}.toMap)

    val pairs = LazyList.iterate
      (template.sliding(2).toList.groupMapReduce(identity)(_ => BigInt("1"))(_+_))
      ( _
        .flatMap[(String, BigInt)]((k,v) => (k(0)+rules(k),v) :: (rules(k)+k(1),v) :: Nil)
        .groupMapReduce(_._1)(_._2)(_+_)
      )

    val count = pairs.map( _
      .toList
      .map[(Char,BigInt)]((k,v) => (k(1),v))
      .prepended((template.head, BigInt("1")))
      .groupMapReduce(_._1)(_._2)(_+_)
    )

    val range = count.map(c => c.values.max - c.values.min)

    println(range(10))

    println(range(40))
}


