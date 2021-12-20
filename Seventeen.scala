object Seventeen extends Input:
{
  val target = input.head match
    case s"target area: x=$x1..$x2, y=$y1..$y2" =>
      ((x1.toInt,y1.toInt),(x2.toInt,y2.toInt))

  def hit(v: (Int,Int)): Boolean = {
    var pos = (0,0)
    var vel = v
    while(pos._1 < target._2._1 && pos._2 > target._1._2) {
      pos = (pos._1 + vel._1, pos._2 + vel._2)
      vel = (vel._1 - vel._1.sign, vel._2 - 1)
      if((target._1._1 to target._2._1 contains pos._1) && (target._1._2 to target._2._2 contains pos._2)) {
        return true
      }
    }
    false
  }
  val hits = for(
    xv <- 0 to target._2._1;
    yv <- -target._1._2.abs until target._1._2.abs
  ) yield hit(xv,yv)


  println(hits.count(identity))
//  println(calc(2,-1))
}
