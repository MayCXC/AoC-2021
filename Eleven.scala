import scala.collection.IntStepper

object Eleven extends Input:
{
  val octopuses = input.map(_.map(_.asDigit).toArray).toArray

  def flash(octo: Array[Array[Int]], flashed: Array[Array[Boolean]])(row: Int, col: Int): Seq[(Int,Int)] =
    if(octo(row)(col) >= 10 && !flashed(row)(col)){
      flashed(row)(col) = true
      for(
        v <- -1 to 1; h <- -1 to 1
        if !(v==0 && h==0) && octo.indices.contains(row+v) && octo(row).indices.contains(col+h)
      ) yield {
        octo(row+v)(col+h) += 1
        (row+v,col+h)
      }
    }
    else Seq.empty

  def step(octo: Array[Array[Int]]): Int =
    val flashed = octo.map(arr => Array.fill(arr.length)(false))

    for(row <- octo.indices; col <- octo(row).indices)
      octo(row)(col) += 1

    LazyList.iterate(
      for(row <- octo.indices; col <- octo(row).indices) yield (row,col)
    )(_.flatMap(flash(octo,flashed))).find(_.isEmpty)

    for(row <- octo.indices; col <- octo(row).indices)
      if(flashed(row)(col))
        octo(row)(col) = 0

    flashed.flatten.count(identity)

  println( LazyList.iterate((octopuses.transpose.transpose,0))((o,i) => (o,step(o)))
    .take(101).map(_._2).sum
  )

  println( LazyList.iterate((octopuses.transpose.transpose,0,0))((o,i,j) => (o,step(octopuses),j+1))
    .find((o,i,j) => i == 100).get._3
  )
}
