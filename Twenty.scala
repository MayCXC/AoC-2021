import scala.collection.mutable

object Twenty extends Input:
{
  val rules = input.head.indices.filter(input.head(_).equals('#')).toSet

  val image = input.tail.tail

  val light = (for(
    row <- image.indices;
    col <- image(row).indices;
    if image(row)(col).equals('#')
  ) yield (row,col)).toSet

  def flash(prev: Set[(Int,Int)]): Set[(Int,Int)] = {
    def check(pixels: Set[(Int,Int)], cmp: Boolean)(y: Int, x: Int) = rules(
      (for (v <- -1 to 1; h <- -1 to 1) yield pixels(y + v, x + h))
        .foldLeft(0) { case (l, r) => l << 1 | (r == cmp).compareTo(false) }
    )

    def bound(pixels: Set[(Int,Int)]): Set[(Int,Int)] = ( for(
        y <- pixels.map(_._1).min - 1 to pixels.map(_._1).max + 1;
        x <- pixels.map(_._2).min - 1 to pixels.map(_._2).max + 1
      ) yield (y,x) ).toSet

    val dark = bound(prev).filterNot(check(prev,true))

    bound(dark).filter(check(dark,false))
  }

  println(LazyList.iterate(light)(flash)(1).size)

  println(LazyList.iterate(light)(flash)(25).size)
}
