object Five extends Input:
{
  val diagram = scala.collection.mutable.Map[(Int,Int),Int]().withDefaultValue(0)

  val vents = input.map{
    case s"$x1,$y1 -> $x2,$y2" => (x1.toInt,y1.toInt) -> (x2.toInt,y2.toInt)
  }

  vents.foreach{ case (x1,y1) -> (x2,y2) =>
    if(x1 == x2) (y1 toby y2).foreach(y => diagram((x1,y)) += 1) else
    if(y1 == y2) (x1 toby x2).foreach(x => diagram((x,y1)) += 1)
  }

  println(diagram.count{ case (x,y) -> n => n > 1 })

  vents.foreach{ case (x1,y1) -> (x2,y2) =>
    if(x1 != x2 && y1 != y2) (x1 toby x2).zip(y1 toby y2)
      .foreach((x,y) => diagram((x,y)) += 1)
  }

  println(diagram.count{ case (x,y) -> n => n > 1 })
}
