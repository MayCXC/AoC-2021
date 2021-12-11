object Seven extends Input:
{ // Another approach is https://en.wikipedia.org/wiki/Average_absolute_deviation#Minimization

  val positions = input.head.split(",").map(_.toInt)

  println(positions.map(p => (p-median(positions)._1).abs).sum)
//  println(positions.indices.map(t => positions.map(p => (p-t).abs).sum).min)

  def triangular(n: Int) = n*(n+1)/2

  println(positions.indices.map(t => positions.map(p => triangular((p-t).abs)).sum).min)
}
