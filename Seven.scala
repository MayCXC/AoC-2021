object Seven extends Input:
{
  // https://en.wikipedia.org/wiki/Average_absolute_deviation#Minimization
  /*
  def tad(l: List[Int]): Double => Double =
    x => l.map(n=>(n-x).abs).sum

  def median(f: Double => Double): List[Int] => Double = {
    case List() => Double.NaN
    case List(a) => a
    case List(a, b) => (a + b).toDouble/2
    case a :: b :: c =>
      if(a == b) median(f)(a :: c.filterNot(b.equals)) else {
        val (x1,x2) = if(a<b) (a,b) else (b,a)
        val (y1,y2) = (f(x1), f(x2))
        if(y1<y2) median(f)(x1 :: c.filterNot(x2.equals)) else
        if(y1>y2) median(f)(x2 :: c.filterNot(x1.equals)) else
          median(f)(a :: b :: Nil)
      }
  }
  */
  //  val med = median(tad(positions.toList))(positions.toList)

  val positions = input.head.split(",").map(_.toInt)

  println(positions.indices.map(t => positions.map(p => (p-t).abs).sum).min)

  def triangular(n: Int): Int = n*(n+1)/2

  println(positions.indices.map(t => positions.map(p => triangular((p-t).abs)).sum).min)
}
