object Five extends Input:
{
  val vents = input.map{
    case s"$x1,$y1 -> $x2,$y2" => (x1.toInt,y1.toInt) -> (x2.toInt,y2.toInt)
  }

  val (straight, diagonal) = vents.partition{
    case (x1,y1) -> (x2,y2) => x1==x2 || y1==y2
  }

  val straight_vents = straight.flatMap{
    case (x1,y1) -> (x2,y2) =>
      if(x1==x2) (y1 toby y2).map(y => (x1,y)) else
        (x1 toby x2).map(x => (x,y1))
  }.groupMapReduce(identity)(_ => 1)(_+_)

  println(straight_vents.values.count(_>1))

  val diagonal_vents = diagonal.flatMap{
    case (x1,y1) -> (x2,y2) => (x1 toby x2).zip(y1 toby y2)
  }.groupMapReduce(identity)(_ => 1)(_+_)

  println(diagonal_vents.toList.concat(straight_vents.toList).groupMapReduce(_._1)(_._2)(_+_).values.count(_>1))
}
