object Twelve extends Input:
{
  val paths = input.map{ case s"$l-$r" => (l,r) }.flatMap((x,y) => (x,y) :: (y,x) :: Nil).groupMap(_._1)(_._2)

  def walks(twice: Boolean = false, small: Set[String] = Set.empty, head: String = "start"): Int =
    if(head.equals("end"))
      1
    else if(head.equals(head.toUpperCase))
      paths(head).map(h => walks(twice, small, h)).sum
    else if(!small(head))
      paths(head).map(h => walks(twice, small.incl(head), h)).sum
    else if(!head.equals("start") && twice)
      paths(head).map(h => walks(false, small.incl(head), h)).sum
    else
      0

  println(walks())

  println(walks(true))
}
