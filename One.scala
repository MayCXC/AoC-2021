object One extends Input {
  {
    val single = input.map(_.toInt).sliding(2).count { case Seq(x, y) => x < y }

    println(single)

    val window = input.map(_.toInt).sliding(4).count { case Seq(w, x, y, z) => w < z }

    println(window)
  }
}
