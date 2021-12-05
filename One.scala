object One extends Input:
{
  val depths = input.map(_.toInt)
  println(depths.sliding(2).count{ case Seq(x, y) => x < y })
  println(depths.sliding(4).count{ case Seq(w, x, y, z) => w < z })
}
