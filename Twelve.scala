object Twelve extends Input:
{
  val paths = input.map{
    case s"$l-$r" => (l,r)
  }.flatMap((x,y) => (x,y) :: (y,x) :: Nil).groupMap(_._1)(_._2)

  def walks(small: Set[String], twice: Boolean, path: List[String]): Int =
    if(path.head.equals("end"))
      1
    else if(path.head.equals(path.head.toUpperCase))
      paths(path.head).map(p => walks(small, twice, p :: path)).sum
    else if(!small(path.head))
      paths(path.head).map(p => walks(small.incl(path.head), twice, p :: path)).sum
    else if(!path.head.equals("start") && !twice)
      paths(path.head).map(p => walks(small.incl(path.head), true, p :: path)).sum
    else 0

  println(walks(Set.empty,true,"start" :: Nil))

  println(walks(Set.empty,false,"start" :: Nil))
}
