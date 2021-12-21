object TwentyOne extends Input:
{
  val starts = input.map {
    case s"Player $n starting position: $p" => n.toInt -> p.toInt
  }.toMap

  val deterministic: LazyList[Int] = LazyList.from(1 to 100) #::: deterministic

  def practice(posOne: Int, posTwo: Int, scoreOne: Int, scoreTwo: Int, turn: Int, die: Iterator[Int]): Int = {
    if(scoreOne >= 1000 || scoreTwo >= 1000) Math.min(scoreOne,scoreTwo)*turn*3 else {
      val roll = die.take(3).sum

      val po = if (turn % 2 == 0) Math.floorMod(posOne + roll - 1, 10) + 1 else posOne
      val pt = if (turn % 2 == 1) Math.floorMod(posTwo + roll - 1, 10) + 1 else posTwo

      val so = if (turn % 2 == 0) scoreOne + po else scoreOne
      val st = if (turn % 2 == 1) scoreTwo + pt else scoreTwo

      practice(po,pt,so,st,turn+1,die)
    }
  }

  println(practice(starts(1),starts(2),0,0,0,deterministic.iterator))

  def quantum = (for (i <- 1 to 3; j <- 1 to 3; k <- 1 to 3) yield i + j + k)
    .groupMapReduce(identity)(_ => 1)(_ + _)

  def dirac(posOne: Int, posTwo: Int, scoreOne: BigInt, scoreTwo: BigInt, turn: Int, universes: BigInt): (BigInt, BigInt) =
    if (scoreOne >= 21) (universes, 0) else
    if (scoreTwo >= 21) (0, universes) else
      (for ((roll,count) <- quantum.toList) yield { // why does this break without .toList !?
        val po = if (turn % 2 == 0) Math.floorMod(posOne + roll - 1, 10) + 1 else posOne
        val pt = if (turn % 2 == 1) Math.floorMod(posTwo + roll - 1, 10) + 1 else posTwo

        val so = if (turn % 2 == 0) scoreOne + po else scoreOne
        val st = if (turn % 2 == 1) scoreTwo + pt else scoreTwo

        dirac(po, pt, so, st, turn + 1, universes * count)
      }).reduce { case ((l1, r1), (l2, r2)) => ((l1 + l2), (r1 + r2)) }

  println(dirac(starts(1),starts(2),0,0,0,1) match { case (l,r) => l.max(r) })
}