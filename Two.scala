object Two extends Input:
{
  val parse = input.map{
    case s"forward $n" => (n.toInt, 0)
    case s"down $n" => (0, n.toInt)
    case s"up $n" => (0, -n.toInt)
  }

  val (pos, dep) = parse.reduce{
    case ((x1, y1), (x2, y2)) => (x1 + x2, y1 + y2)
  }

  println(pos * dep)

  val (pos_aim, dep_aim, _) = parse.foldLeft((0, 0, 0)) {
    case ((x1, y1, a), (x2, y2)) =>
      if (y2 != 0) (x1, y1, a + y2) else
      if (x2 != 0) (x1 + x2, y1 + a * x2, a) else
        (x1, y1, a)
  }

  println(pos_aim * dep_aim)
}
