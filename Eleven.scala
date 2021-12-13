import scala.collection.IntStepper

object Eleven extends Input:
{
  val octopuses = input.map(_.map(_.asDigit).toArray).toArray

  def step(grid: Array[Array[Int]]): Int =
    val flashed = grid.map(array => Array.fill(array.length)(false))

    def flash(row: Int, col: Int): Unit =
      grid(row)(col) += 1

      if(grid(row)(col) >= 10 && !flashed(row)(col))
        flashed(row)(col) = true

        for(v <- -1 to 1; h <- -1 to 1 if
          !(v==0 && h==0) && grid.indices.contains(row+v) && grid(row).indices.contains(col+h)
        ) flash(row+v,col+h)

    for(row <- grid.indices; col <- grid(row).indices)
      flash(row,col)

    for(row <- grid.indices; col <- grid(row).indices)
      if(flashed(row)(col))
        grid(row)(col) = 0

    flashed.flatten.count(identity)

  val simulator = LazyList.continually(step(octopuses))

  println(simulator.take(100).sum)

  simulator.zipWithIndex.find((x,y) => x == 100).foreach((x,y) => println(y+1))
}
