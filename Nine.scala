object Nine extends Input:
{
  val low = input.indices.flatMap(y => input(y).indices.map(x => (y,x)))
    .filter( (y,x) => List((-1, 0), (1, 0), (0, -1), (0, 1))
      .map((v, h) => (y+v, x+h))
      .filter((yv, xh) => input.indices.contains(yv) && input(yv).indices.contains(xh))
      .forall((yv, xh) => input(y)(x).asDigit < input(yv)(xh).asDigit)
    )

  println(low.map((y,x)=>input(y)(x).asDigit+1).sum)

  def flood(hm: Array[Array[Int]])(y: Int, x: Int): List[(Int,Int)] =
      if(hm.indices.contains(y) && hm(y).indices.contains(x) && hm(y)(x) < 9) {
        hm(y)(x) = 9
        (y, x) :: List((-1, 0), (1, 0), (0, -1), (0, 1)).flatMap((v, h) => flood(hm)(y+v, x+h))
      }
      else Nil

  val heightmap = input.map(_.map(_.asDigit).toArray).toArray
  val Seq(one, two, three, _*) = low.map(flood(heightmap).tupled.andThen(_.length)).sortWith(_>=_)

  println(one*two*three)
}
